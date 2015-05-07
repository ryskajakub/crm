module Crm.Server.Api.UpkeepResource (
  insertUpkeepMachines ,
  upkeepResource ) where

import Opaleye.Operators ((.==))
import Opaleye.Manipulation (runInsert, runUpdate, runDelete)
import Opaleye.PGTypes (pgDay, pgBool, pgInt4, pgString)
import Opaleye (runQuery)

import Database.PostgreSQL.Simple (Connection)

import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Error.Class (throwError)
import Control.Monad (forM_)

import Data.Tuple.All (sel1, sel2, sel3, sel4)

import Rest.Types.Error (Reason(NotAllowed))
import Rest.Resource (Resource, Void, schema, list, name, mkResourceReaderWith, get, update, remove)
import qualified Rest.Schema as S
import Rest.Dictionary.Combinators (jsonO, jsonI)
import Rest.Handler (ListHandler, mkListing, Handler, mkConstHandler, mkInputHandler)

import qualified Crm.Shared.Api as A
import qualified Crm.Shared.Upkeep as U
import qualified Crm.Shared.Employee as E
import qualified Crm.Shared.Machine as M
import qualified Crm.Shared.UpkeepMachine as UM
import Crm.Shared.MyMaybe

import Crm.Server.Helpers (prepareReaderTuple, withConnId, readMay', createDeletion ,
  ymdToDay, maybeToNullable, deleteRows')
import Crm.Server.Boilerplate ()
import Crm.Server.Types
import Crm.Server.DB

data UpkeepsListing = UpkeepsAll | UpkeepsPlanned

insertUpkeepMachines :: Connection -> U.UpkeepId -> [(UM.UpkeepMachine, M.MachineId)] -> IO ()
insertUpkeepMachines connection upkeepId upkeepMachines = let
  insertUpkeepMachine (upkeepMachine', upkeepMachineId) = do
    _ <- runInsert
      connection
      upkeepMachinesTable (
        pgInt4 $ U.getUpkeepId upkeepId ,
        pgString $ UM.upkeepMachineNote upkeepMachine' ,
        pgInt4 $ M.getMachineId upkeepMachineId ,
        pgInt4 $ UM.recordedMileage upkeepMachine' , 
        pgBool $ UM.warrantyUpkeep upkeepMachine' )
    return ()
  in forM_ upkeepMachines insertUpkeepMachine

removeUpkeep :: Handler IdDependencies
removeUpkeep = deleteRows' [createDeletion upkeepMachinesTable, createDeletion upkeepsTable]

upkeepResource :: Resource Dependencies IdDependencies UrlId UpkeepsListing Void
upkeepResource = (mkResourceReaderWith prepareReaderTuple) {
  list = \listingType -> case listingType of
    UpkeepsAll -> upkeepListing
    UpkeepsPlanned -> upkeepsPlannedListing ,
  name = A.upkeep ,
  update = Just updateUpkeepHandler ,
  schema = upkeepSchema ,
  remove = Just removeUpkeep ,
  get = Just upkeepCompanyMachines }

updateUpkeepHandler :: Handler IdDependencies
updateUpkeepHandler = mkInputHandler (jsonO . jsonI) (\(upkeep,machines,employeeId) -> let 
  upkeepTriple = (upkeep, machines, toMaybe employeeId)
  in withConnId (\connection upkeepId ->
    liftIO $ updateUpkeep connection (U.UpkeepId upkeepId) upkeepTriple))

updateUpkeep :: Connection
             -> U.UpkeepId
             -> (U.Upkeep, [(UM.UpkeepMachine, M.MachineId)], Maybe E.EmployeeId)
             -> IO ()
updateUpkeep conn upkeepId (upkeep, upkeepMachines, employeeId) = do
  _ <- let
    condition (upkeepId',_,_,_,_,_,_) = upkeepId' .== pgInt4 (U.getUpkeepId upkeepId)
    readToWrite _ =
      (Nothing, pgDay $ ymdToDay $ U.upkeepDate upkeep, pgBool $ U.upkeepClosed upkeep, 
        maybeToNullable $ (pgInt4 . E.getEmployeeId) `fmap` employeeId, pgString $ U.workHours upkeep, 
        pgString $ U.workDescription upkeep, pgString $ U.recommendation upkeep)
    in runUpdate conn upkeepsTable readToWrite condition
  _ <- runDelete conn upkeepMachinesTable (\(upkeepId',_,_,_,_) -> upkeepId' .== (pgInt4 $ U.getUpkeepId upkeepId))
  insertUpkeepMachines conn upkeepId upkeepMachines
  return ()

upkeepListing :: ListHandler Dependencies
upkeepListing = mkListing jsonO (const $ do
  rows <- ask >>= \conn -> liftIO $ runQuery conn expandedUpkeepsQuery
  return $ mapUpkeeps rows) 

upkeepsPlannedListing :: ListHandler Dependencies
upkeepsPlannedListing = mkListing jsonO (const $ do
  conn <- ask
  rows <- liftIO $ runQuery conn groupedPlannedUpkeepsQuery
  return $ map (\row -> let
    (u, c) = convertDeep row :: (UpkeepMapped, CompanyMapped)
    in (sel1 u, sel3 u, sel1 c, sel2 c)) rows)

upkeepSchema :: S.Schema UrlId UpkeepsListing Void
upkeepSchema = S.withListing UpkeepsAll (S.named [
  (A.planned, S.listing UpkeepsPlanned) ,
  (A.single, S.singleBy readMay')])
    
upkeepCompanyMachines :: Handler IdDependencies
upkeepCompanyMachines = mkConstHandler jsonO $ withConnId (\conn upkeepId -> do
  upkeeps <- liftIO $ fmap mapUpkeeps (runQuery conn $ expandedUpkeepsQuery2 upkeepId)
  upkeep <- singleRowOrColumn upkeeps
  machines <- liftIO $ runMachinesInCompanyByUpkeepQuery upkeepId conn
  companyId <- case machines of
    [] -> throwError NotAllowed
    (companyId',_) : _ -> return companyId'
  return (companyId, (sel2 upkeep, sel3 upkeep, sel4 upkeep), map snd machines))
