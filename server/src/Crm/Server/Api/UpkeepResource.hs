{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Crm.Server.Api.UpkeepResource (
  loadNextServiceTypeHint ,
  insertUpkeepMachines ,
  printDailyPlanListing' ,
  upkeepResource ) where

import           Opaleye.Operators           ((.==))
import           Opaleye.Manipulation        (runInsert, runUpdate, runDelete, runInsertReturning, runInsert)
import           Opaleye.PGTypes             (pgDay, pgBool, pgInt4, pgStrictText)
import           Opaleye                     (runQuery)

import qualified Text.Parsec as P

import           Database.PostgreSQL.Simple  (Connection)

import           Control.Monad.Reader        (ask)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Error.Class   (throwError)
import           Control.Monad               (forM_, forM)
import           Control.Monad.Trans.Except  (ExceptT)
import           Control.Arrow               (second)

import           Data.Tuple.All              (sel1, sel2, sel3, uncurryN)
import           Data.List                   (nub)
import           Data.Text                   (intercalate, pack, Text)
import           Data.Time.Calendar          (fromGregorian, Day)
import           Data.Pool                   (withResource)

import           Rest.Types.Error            (Reason(NotAllowed, CustomReason), DomainReason(DomainReason), DataError(ParseError, MissingField))
import           Rest.Resource               (Resource, Void, schema, list, name, 
                                             mkResourceReaderWith, get, update, remove, create)
import qualified Rest.Schema                 as S
import           Rest.Dictionary.Combinators (jsonO, jsonI, mkPar)
import           Rest.Dictionary.Types       (Dict, Param(..))
import           Rest.Handler                (ListHandler, Handler, Env(param))

import qualified Crm.Shared.Api              as A
import qualified Crm.Shared.Upkeep           as U
import qualified Crm.Shared.Employee         as E
import qualified Crm.Shared.Machine          as M
import qualified Crm.Shared.MachineType      as MT
import qualified Crm.Shared.UpkeepMachine    as UM
import qualified Crm.Shared.UpkeepSequence   as US
import qualified Crm.Shared.ContactPerson    as CP
import qualified Crm.Shared.ServerRender     as SR
import qualified Crm.Shared.Company          as C
import           Crm.Shared.MyMaybe

import           Crm.Server.Helpers          (prepareReaderTuple, createDeletion, ymdToDay, catchError)
import           Crm.Server.Boilerplate      ()
import           Crm.Server.Types
import           Crm.Server.DB
import           Crm.Server.Handler          (mkInputHandler', mkConstHandler', mkListing', deleteRows'', 
                                             mkGenHandler', mkDayParam, getDayParam)
import           Crm.Server.CachedCore       (recomputeWhole)
import           Crm.Server.Core             (nextServiceTypeHint)
import           Crm.Server.Parsers          (parseMarkup)

import           TupleTH                     (proj, catTuples)


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
  (cache, pool) <- ask
  -- todo check that the machines are belonging to this company
  upkeepId <- withResource pool $ \connection -> liftIO $ addUpkeep connection newUpkeep
  recomputeWhole pool cache
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
  deleteRows'' [createDeletion upkeepEmployeesTable, createDeletion upkeepMachinesTable, createDeletion upkeepsTable]
    upkeepIdInt connection

updateUpkeepHandler :: Handler (IdDependencies' U.UpkeepId)
updateUpkeepHandler = mkInputHandler' (jsonO . jsonI) $ \(upkeep, machines, employeeIds) -> let 
  upkeepTuple = (upkeep, machines)
  in do 
    ((cache, pool), upkeepId) <- ask
    withResource pool $ \connection -> liftIO $ 
      updateUpkeep connection upkeepId upkeepTuple employeeIds
    recomputeWhole pool cache

updateUpkeep :: Connection
             -> U.UpkeepId
             -> (U.Upkeep, [(UM.UpkeepMachine, M.MachineId)])
             -> [E.EmployeeId]
             -> IO ()
updateUpkeep conn upkeepId (upkeep, upkeepMachines) employeeIds = do
  _ <- let
    condition upkeepRow = $(proj 6 0) upkeepRow .== pgInt4 (U.getUpkeepId upkeepId)
    readToWrite u =
      (Just . $(proj 6 0) $ u, pgDay $ ymdToDay $ U.upkeepDate upkeep, pgBool $ U.upkeepClosed upkeep, 
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
  (_, pool) <- ask 
  rows <- withResource pool $ \connection -> liftIO $ runQuery connection expandedUpkeepsQuery
  return . mapUpkeeps $ rows

upkeepsPlannedListing :: ListHandler Dependencies
upkeepsPlannedListing = mkListing' jsonO $ const $ do
  (_,pool) <- ask
  rows <- liftIO $ withResource pool $ \connection -> runQuery connection groupedPlannedUpkeepsQuery
  (upkeeps' :: [(U.UpkeepId, U.Upkeep, C.CompanyId, C.Company, [(M.MachineId, Text, Text)])]) <- 
      liftIO $ forM rows $ \(upkeepRowPart, (companyId :: C.CompanyId, company :: C.Company)) -> do
    let (u :: UpkeepMapped) = convert upkeepRowPart
    let upkeepId = $(proj 2 0) u
    notes <- withResource pool $ \connection -> runQuery connection (notesForUpkeep . U.getUpkeepId $ upkeepId)
    return (upkeepId, $(proj 2 1) u, companyId, company, fmap (\(a,b,c) -> (M.MachineId a,b,c)) notes :: [(M.MachineId, Text, Text)])
  employeeRows <- liftIO $ withResource pool $ \connection -> runQuery connection 
    (employeesInUpkeeps (fmap $(proj 5 0) upkeeps'))
  let 
    (employees :: [(U.UpkeepId, EmployeeMapped)]) = (\(uId, e) -> (U.UpkeepId uId, convert e)) `fmap` employeeRows
    upkeepsWithEmployees = map (\tuple -> let
      employee = map snd . filter (($(proj 5 0) tuple ==) . fst) $ employees
      in $(catTuples 5 1) tuple employee) upkeeps'
  return upkeepsWithEmployees
    
upkeepCompanyMachines :: Handler (IdDependencies' U.UpkeepId)
upkeepCompanyMachines = mkConstHandler' jsonO $ do
  ((_, pool), upkeepId) <- ask
  let U.UpkeepId upkeepIdInt = upkeepId
  upkeeps <- withResource pool $ \connection -> liftIO $ fmap mapUpkeeps (runQuery connection $ expandedUpkeepsQuery2 upkeepIdInt)
  upkeep <- singleRowOrColumn upkeeps

  machines' <- withResource pool $ \connection -> liftIO $ runQuery connection (machinesInUpkeepQuery' upkeepIdInt)
  otherMachines <- withResource pool $ \connection -> liftIO $ runQuery connection (machinesNotInUpkeepQuery upkeepId)
  
  -- machinesNotInUpkeepQuery 
  employeeIds <- withResource pool $ \connection -> liftIO $ runQuery connection (employeeIdsInUpkeep upkeepIdInt)
  let machines = map (\(m, mt) -> (convert m :: MachineMapped, convert mt :: MachineTypeMapped)) (machines' ++ otherMachines)
  machines'' <- withResource pool $ \connection -> loadNextServiceTypeHint machines connection

  companyId <- case machines of
    [] -> throwError NotAllowed
    (machineMapped,_) : _ -> return $ $(proj 6 1) machineMapped

  return (companyId, (sel2 upkeep, sel3 upkeep, fmap E.EmployeeId employeeIds), 
    map (\(m, mt, nextUpkeepSequence) -> ($(proj 6 0) m, $(proj 6 5) m, $(proj 2 1) mt, nextUpkeepSequence)) machines'')

loadNextServiceTypeHint :: (MonadIO m)
                        => [(MachineMapped, MachineTypeMapped)] 
                        -> Connection 
                        -> ExceptT (Reason Text) m [(MachineMapped, MachineTypeMapped, US.UpkeepSequence)]
loadNextServiceTypeHint machines conn = forM machines $ \(machine, machineType) -> do
  upkeepSequences' <- liftIO $ runQuery conn (upkeepSequencesByIdQuery (pgInt4 . MT.getMachineTypeId . $(proj 2 0) $ machineType))
  pastUpkeepMachines' <- liftIO $ runQuery conn (pastUpkeepMachinesQ (M.getMachineId . $(proj 6 0) $ machine))
  let upkeepSequences = map (\x -> ($(proj 2 1)) $ (convert x :: UpkeepSequenceMapped)) upkeepSequences'
  let pastUpkeepMachines = map (\x -> ($(proj 3 2)) $ (convert x :: UpkeepMachineMapped)) pastUpkeepMachines'
  uss <- case upkeepSequences of
    (us' : uss') -> return (us', uss')
    _ -> throwError . CustomReason . DomainReason . pack $ "Db in invalid state"
  return (machine, machineType, nextServiceTypeHint uss pastUpkeepMachines)

printDailyPlanListing' :: (MonadIO m, Functor m) 
                       => Maybe E.EmployeeId
                       -> Connection
                       -> Day
                       -> ExceptT (Reason r) m [(U.UpkeepMarkup, C.Company, [(E.EmployeeId, E.Employee)], [(M.Machine, MT.MachineType, MyMaybe CP.ContactPerson, (UM.UpkeepMachine, MyMaybe [SR.Markup]))])]
printDailyPlanListing' employeeId connection day = do
  dailyPlanUpkeeps' <- liftIO $ runQuery connection (dailyPlanQuery employeeId day)
  dailyPlanUpkeeps <- forM dailyPlanUpkeeps' $ \(upkeepMapped, es) -> do
    let 
      upkeep = convert upkeepMapped :: UpkeepMapped
      listifyNote (um @ (UM.UpkeepMachine {})) = 
        (um, catchError . parseMarkup . UM.upkeepMachineNote $ um) where
      upkeepRaw = $(proj 2 1) upkeep
      upkeepMarkup = upkeepRaw {
        U.workDescription = maybe ((:[]) . SR.PlainText . U.workDescription $ upkeepRaw) (\x -> x) . 
          catchError . parseMarkup . U.workDescription $ upkeepRaw }
    employees <- fmap (map $ \e -> let 
      employee = (convert e :: EmployeeMapped) 
      in ($(proj 2 0) employee, $(proj 2 1) employee)) $ 
      liftIO $ runQuery connection (multiEmployeeQuery es)
    machines <- fmap (map $ \(m, mt, cp, um) -> (
      $(proj 6 5) $ (convert m :: MachineMapped) , 
      $(proj 2 1) $ (convert mt :: MachineTypeMapped) ,
      toMyMaybe . $(proj 3 2) $ (convert cp :: MaybeContactPersonMapped) ,
      second toMyMaybe . listifyNote . $(proj 3 2) $ (convert um :: UpkeepMachineMapped))) $ 
      liftIO $ runQuery connection (machinesInUpkeepQuery'' ($(proj 2 0) upkeep))
    (company :: C.Company) <-
      singleRowOrColumn =<< (liftIO $ runQuery connection (companyInUpkeepQuery . $(proj 2 0) $ upkeep))
    return (upkeepMarkup, company, employees, machines)
  return dailyPlanUpkeeps

printDailyPlanListing :: ListHandler Dependencies
printDailyPlanListing = mkGenHandler' (jsonO . mkDayParam) $ \env -> do
  let day = getDayParam env
  (_, pool) <- ask
  withResource pool $ \connection -> printDailyPlanListing' Nothing connection day

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
