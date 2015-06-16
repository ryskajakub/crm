{-# LANGUAGE TemplateHaskell #-}

module Crm.Server.Api.UpkeepResource (
  insertUpkeepMachines ,
  upkeepResource ) where

import           Opaleye.Operators           ((.==))
import           Opaleye.Manipulation        (runInsert, runUpdate, runDelete, runInsertReturning, runInsert)
import           Opaleye.PGTypes             (pgDay, pgBool, pgInt4, pgStrictText)
import           Opaleye                     (runQuery)

import           Database.PostgreSQL.Simple  (Connection)

import           Control.Monad.Reader        (ask)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Error.Class   (throwError)
import           Control.Monad               (forM_)
import           Control.Lens                (over, mapped, _3)

import           Data.Tuple.All              (sel1, sel2, sel3, sel4, upd3)
import           Data.List                   (nub)

import           Rest.Types.Error            (Reason(NotAllowed))
import           Rest.Resource               (Resource, Void, schema, list, name, 
                                             mkResourceReaderWith, get, update, remove, create)
import qualified Rest.Schema                 as S
import           Rest.Dictionary.Combinators (jsonO, jsonI)
import           Rest.Handler                (ListHandler, Handler)

import qualified Crm.Shared.Api              as A
import qualified Crm.Shared.Upkeep           as U
import qualified Crm.Shared.Employee         as E
import qualified Crm.Shared.Machine          as M
import qualified Crm.Shared.UpkeepMachine    as UM
import           Crm.Shared.MyMaybe

import           Crm.Server.Helpers          (prepareReaderTuple, createDeletion, ymdToDay, maybeToNullable)
import           Crm.Server.Boilerplate      ()
import           Crm.Server.Types
import           Crm.Server.DB
import           Crm.Server.Handler          (mkInputHandler', mkConstHandler', mkListing', deleteRows'')
import           Crm.Server.CachedCore       (recomputeWhole)

import           TupleTH                     (proj)


data UpkeepsListing = UpkeepsAll | UpkeepsPlanned


addUpkeep :: Connection
          -> (U.Upkeep, [(UM.UpkeepMachine, M.MachineId)], [E.EmployeeId])
          -> IO U.UpkeepId -- ^ id of the upkeep
addUpkeep connection (upkeep, upkeepMachines, employeeIds) = do
  upkeepIds <- runInsertReturning
    connection
    upkeepsTable (Nothing, pgDay $ ymdToDay $ U.upkeepDate upkeep,
      pgBool $ U.upkeepClosed upkeep, pgStrictText $ U.workHours upkeep, 
      pgStrictText $ U.workDescription upkeep , pgStrictText $ U.recommendation upkeep)
    sel1
  let upkeepId = U.UpkeepId $ head upkeepIds
  insertUpkeepMachines connection upkeepId upkeepMachines
  insertEmployees connection upkeepId (nub employeeIds)
  return upkeepId

insertEmployees :: Connection -> U.UpkeepId -> [E.EmployeeId] -> IO ()
insertEmployees connection upkeepId employeeIds =
  forM_ employeeIds $ \employeeId ->
    runInsert connection upkeepEmployeesTable (pgInt4 . U.getUpkeepId $ upkeepId, pgInt4 . E.getEmployeeId $ employeeId)

createUpkeepHandler :: Handler Dependencies
createUpkeepHandler = mkInputHandler' (jsonO . jsonI) $ \newUpkeep -> do
  (cache, connection) <- ask
  -- todo check that the machines are belonging to this company
  upkeepId <- liftIO $ addUpkeep connection newUpkeep
  recomputeWhole connection cache
  return upkeepId

insertUpkeepMachines :: Connection -> U.UpkeepId -> [(UM.UpkeepMachine, M.MachineId)] -> IO ()
insertUpkeepMachines connection upkeepId upkeepMachines = let
  insertUpkeepMachine (upkeepMachine', upkeepMachineId) = do
    _ <- runInsert
      connection
      upkeepMachinesTable (
        pgInt4 $ U.getUpkeepId upkeepId ,
        pgStrictText $ UM.upkeepMachineNote upkeepMachine' ,
        pgInt4 $ M.getMachineId upkeepMachineId ,
        pgInt4 $ UM.recordedMileage upkeepMachine' , 
        pgBool $ UM.warrantyUpkeep upkeepMachine' )
    return ()
  in forM_ upkeepMachines insertUpkeepMachine

removeUpkeep :: Handler (IdDependencies' U.UpkeepId)
removeUpkeep = mkConstHandler' jsonO $ do
  ((_, connection), U.UpkeepId upkeepIdInt) <- ask
  deleteRows'' [createDeletion upkeepMachinesTable, createDeletion upkeepsTable]
    upkeepIdInt connection

updateUpkeepHandler :: Handler (IdDependencies' U.UpkeepId)
updateUpkeepHandler = mkInputHandler' (jsonO . jsonI) $ \(upkeep, machines, employeeIds) -> let 
  upkeepTuple = (upkeep, machines)
  in do 
    ((cache, connection), upkeepId) <- ask
    liftIO $ updateUpkeep connection upkeepId upkeepTuple employeeIds
    recomputeWhole connection cache

updateUpkeep :: Connection
             -> U.UpkeepId
             -> (U.Upkeep, [(UM.UpkeepMachine, M.MachineId)])
             -> [E.EmployeeId]
             -> IO ()
updateUpkeep conn upkeepId (upkeep, upkeepMachines) employeeIds = do
  _ <- let
    condition upkeepRow = $(proj 6 0) upkeepRow .== pgInt4 (U.getUpkeepId upkeepId)
    readToWrite _ =
      (Nothing, pgDay $ ymdToDay $ U.upkeepDate upkeep, pgBool $ U.upkeepClosed upkeep, 
        pgStrictText $ U.workHours upkeep, 
        pgStrictText $ U.workDescription upkeep, pgStrictText $ U.recommendation upkeep)
    in runUpdate conn upkeepsTable readToWrite condition
  _ <- runDelete conn upkeepMachinesTable (\(upkeepId',_,_,_,_) -> upkeepId' .== (pgInt4 $ U.getUpkeepId upkeepId))
  insertUpkeepMachines conn upkeepId upkeepMachines
  _ <- runDelete conn upkeepEmployeesTable $ \upkeepRow -> $(proj 2 0) upkeepRow .== (pgInt4 . U.getUpkeepId $ upkeepId)
  insertEmployees conn upkeepId employeeIds
  return ()

upkeepListing :: ListHandler Dependencies
upkeepListing = mkListing' jsonO $ const $ do
  rows <- ask >>= \(_,conn) -> liftIO $ runQuery conn expandedUpkeepsQuery
  return . mapUpkeeps $ rows

upkeepsPlannedListing :: ListHandler Dependencies
upkeepsPlannedListing = mkListing' jsonO (const $ do
  (_,conn) <- ask
  rows <- liftIO $ runQuery conn groupedPlannedUpkeepsQuery
  return $ map (\row -> let
    (u, c) = convertDeep row :: (UpkeepMapped, CompanyMapped)
    in ($(proj 2 0) u, $(proj 2 1) u, $(proj 3 0) c, $(proj 3 1) c)) rows)
    
upkeepCompanyMachines :: Handler (IdDependencies' U.UpkeepId)
upkeepCompanyMachines = mkConstHandler' jsonO $ do
  ((_, conn), U.UpkeepId upkeepIdInt) <- ask
  upkeeps <- liftIO $ fmap mapUpkeeps (runQuery conn $ expandedUpkeepsQuery2 upkeepIdInt)
  upkeep <- singleRowOrColumn upkeeps
  machines <- liftIO $ runMachinesInCompanyByUpkeepQuery upkeepIdInt conn
  companyId <- case machines of
    [] -> throwError NotAllowed
    (companyId',_) : _ -> return companyId'
  return (companyId, (sel2 upkeep, sel3 upkeep), map snd machines)


-- resource

upkeepResource :: Resource Dependencies (IdDependencies' U.UpkeepId) U.UpkeepId UpkeepsListing Void
upkeepResource = (mkResourceReaderWith prepareReaderTuple) {
  list = \listingType -> case listingType of
    UpkeepsAll -> upkeepListing
    UpkeepsPlanned -> upkeepsPlannedListing ,
  name = A.upkeep ,
  update = Just updateUpkeepHandler ,
  schema = upkeepSchema ,
  remove = Just removeUpkeep ,
  create = Just createUpkeepHandler ,
  get = Just upkeepCompanyMachines }

upkeepSchema :: S.Schema U.UpkeepId UpkeepsListing Void
upkeepSchema = S.withListing UpkeepsAll (S.named [
  (A.planned, S.listing UpkeepsPlanned) ,
  (A.single, S.singleRead id )])
