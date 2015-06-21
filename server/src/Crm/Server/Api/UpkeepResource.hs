{-# LANGUAGE TemplateHaskell #-}

module Crm.Server.Api.UpkeepResource (
  loadNextServiceTypeHint ,
  insertUpkeepMachines ,
  upkeepResource ) where

import           Opaleye.Operators           ((.==))
import           Opaleye.Manipulation        (runInsert, runUpdate, runDelete, runInsertReturning, runInsert)
import           Opaleye.PGTypes             (pgDay, pgBool, pgInt4, pgStrictText)
import           Opaleye                     (runQuery)

import           Database.PostgreSQL.Simple  (Connection)

import           Control.Monad.Reader        (ask)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Error.Class   (throwError)
import           Control.Monad               (forM_, forM)
import           Control.Monad.Trans.Except  (ExceptT)

import           Data.Tuple.All              (sel1, sel2, sel3)
import           Data.List                   (nub)
import           Data.Text                   (intercalate, pack)
import           Data.Time.Calendar          (fromGregorian)

import           Rest.Types.Error            (Reason(NotAllowed, CustomReason), DomainReason(DomainReason))
import           Rest.Resource               (Resource, Void, schema, list, name, 
                                             mkResourceReaderWith, get, update, remove, create)
import qualified Rest.Schema                 as S
import           Rest.Dictionary.Combinators (jsonO, jsonI)
import           Rest.Handler                (ListHandler, Handler)

import qualified Crm.Shared.Api              as A
import qualified Crm.Shared.Upkeep           as U
import qualified Crm.Shared.Employee         as E
import qualified Crm.Shared.Machine          as M
import qualified Crm.Shared.MachineType      as MT
import qualified Crm.Shared.UpkeepMachine    as UM
import qualified Crm.Shared.UpkeepSequence   as US

import           Crm.Server.Helpers          (prepareReaderTuple, createDeletion, ymdToDay)
import           Crm.Server.Boilerplate      ()
import           Crm.Server.Types
import           Crm.Server.DB
import           Crm.Server.Handler          (mkInputHandler', mkConstHandler', mkListing', deleteRows'')
import           Crm.Server.CachedCore       (recomputeWhole)
import           Crm.Server.Core             (nextServiceTypeHint)

import           TupleTH                     (proj)


data UpkeepsListing = UpkeepsAll | UpkeepsPlanned | PrintDailyPlan


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
  forM_ (employeeIds `zip` [0..]) $ \(employeeId, ordering) ->
    runInsert connection upkeepEmployeesTable (
      pgInt4 . U.getUpkeepId $ upkeepId , 
      pgInt4 . E.getEmployeeId $ employeeId , 
      pgInt4 ordering)

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
        pgBool $ UM.warrantyUpkeep upkeepMachine' ,
        pgStrictText $ UM.endNote upkeepMachine' )
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
  _ <- runDelete conn upkeepMachinesTable $ \upkeepRow -> $(proj 6 0) upkeepRow .== (pgInt4 . U.getUpkeepId $ upkeepId)
  insertUpkeepMachines conn upkeepId upkeepMachines
  _ <- runDelete conn upkeepEmployeesTable $ \upkeepRow -> $(proj 3 0) upkeepRow .== (pgInt4 . U.getUpkeepId $ upkeepId)
  insertEmployees conn upkeepId employeeIds
  return ()

upkeepListing :: ListHandler Dependencies
upkeepListing = mkListing' jsonO $ const $ do
  rows <- ask >>= \(_,conn) -> liftIO $ runQuery conn expandedUpkeepsQuery
  return . mapUpkeeps $ rows

upkeepsPlannedListing :: ListHandler Dependencies
upkeepsPlannedListing = mkListing' jsonO $ const $ do
  (_,conn) <- ask
  rows <- liftIO $ runQuery conn groupedPlannedUpkeepsQuery
  liftIO $ forM rows $ \row -> do
    let (u, c) = convertDeep row :: (UpkeepMapped, CompanyMapped)
    let upkeepId = $(proj 2 0) u
    notes <- runQuery conn (notesForUpkeep . U.getUpkeepId $ upkeepId)
    let note = intercalate (pack " | ") notes
    return (upkeepId, $(proj 2 1) u, $(proj 3 0) c, $(proj 3 1) c, note)
    
upkeepCompanyMachines :: Handler (IdDependencies' U.UpkeepId)
upkeepCompanyMachines = mkConstHandler' jsonO $ do
  ((_, conn), U.UpkeepId upkeepIdInt) <- ask
  upkeeps <- liftIO $ fmap mapUpkeeps (runQuery conn $ expandedUpkeepsQuery2 upkeepIdInt)
  upkeep <- singleRowOrColumn upkeeps

  machines' <- liftIO $ runQuery conn (machinesInUpkeepQuery' upkeepIdInt)
  employeeIds <- liftIO $ runQuery conn (employeeIdsInUpkeep upkeepIdInt)
  let machines = map (\(m, mt) -> (convert m :: MachineMapped, convert mt :: MachineTypeMapped)) machines'
  machines'' <- loadNextServiceTypeHint machines conn

  companyId <- case machines of
    [] -> throwError NotAllowed
    (machineMapped,_) : _ -> return $ $(proj 6 1) machineMapped

  return (companyId, (sel2 upkeep, sel3 upkeep, fmap E.EmployeeId employeeIds), 
    map (\(m, mt, nextUpkeepSequence) -> ($(proj 6 0) m, $(proj 6 5) m, $(proj 2 1) mt, nextUpkeepSequence)) machines'')

loadNextServiceTypeHint :: (MonadIO m)
                        => [(MachineMapped, MachineTypeMapped)] 
                        -> Connection 
                        -> ExceptT (Reason String) m [(MachineMapped, MachineTypeMapped, US.UpkeepSequence)]
loadNextServiceTypeHint machines conn = forM machines $ \(machine, machineType) -> do
  upkeepSequences' <- liftIO $ runQuery conn (upkeepSequencesByIdQuery (pgInt4 . MT.getMachineTypeId . $(proj 2 0) $ machineType))
  pastUpkeepMachines' <- liftIO $ runQuery conn (pastUpkeepMachinesQ (M.getMachineId . $(proj 6 0) $ machine))
  let upkeepSequences = map (\x -> ($(proj 2 1)) $ (convert x :: UpkeepSequenceMapped)) upkeepSequences'
  let pastUpkeepMachines = map (\x -> ($(proj 3 2)) $ (convert x :: UpkeepMachineMapped)) pastUpkeepMachines'
  uss <- case upkeepSequences of
    (us' : uss') -> return (us', uss')
    _ -> throwError $ CustomReason $ DomainReason "Db in invalid state"
  return (machine, machineType, nextServiceTypeHint uss pastUpkeepMachines)

printDailyPlanListing :: ListHandler Dependencies
printDailyPlanListing = mkListing' jsonO $ const $ do
  (_, connection) <- ask
  dailyPlanUpkeeps' <- liftIO $ runQuery connection (dailyPlanQuery Nothing (fromGregorian 2015 6 17))
  dailyPlanUpkeeps <- forM dailyPlanUpkeeps' $ \(upkeepMapped, es) -> do
    let upkeep = convert upkeepMapped :: UpkeepMapped
    employees <- fmap (map $ \e -> $(proj 2 1) (convert e :: EmployeeMapped)) $ 
      liftIO $ runQuery connection (multiEmployeeQuery es)
    machines <- fmap (map $ \(m, mt, cp, um) -> (
      $(proj 6 5) $ (convert m :: MachineMapped) , 
      $(proj 2 1) $ (convert mt :: MachineTypeMapped) ,
      $(proj 3 2) $ (convert cp :: ContactPersonMapped) ,
      $(proj 3 2) $ (convert um :: UpkeepMachineMapped) )) $ 
      liftIO $ runQuery connection (machinesInUpkeepQuery'' ($(proj 2 0) upkeep))
    company <- fmap (\c -> $(proj 3 1) (convert c :: CompanyMapped)) $ 
      singleRowOrColumn =<< (liftIO $ runQuery connection (companyInUpkeepQuery . $(proj 2 0) $ upkeep))
    return ($(proj 2 1) upkeep, company, employees, machines)
  return dailyPlanUpkeeps


-- resource

upkeepResource :: Resource Dependencies (IdDependencies' U.UpkeepId) U.UpkeepId UpkeepsListing Void
upkeepResource = (mkResourceReaderWith prepareReaderTuple) {
  list = \listingType -> case listingType of
    UpkeepsAll -> upkeepListing
    UpkeepsPlanned -> upkeepsPlannedListing
    PrintDailyPlan -> printDailyPlanListing ,
  name = A.upkeep ,
  update = Just updateUpkeepHandler ,
  schema = upkeepSchema ,
  remove = Just removeUpkeep ,
  create = Just createUpkeepHandler ,
  get = Just upkeepCompanyMachines }

upkeepSchema :: S.Schema U.UpkeepId UpkeepsListing Void
upkeepSchema = S.withListing UpkeepsAll (S.named [
  (A.planned, S.listing UpkeepsPlanned) ,
  ("print", S.listing PrintDailyPlan) ,
  (A.single, S.singleRead id )])
