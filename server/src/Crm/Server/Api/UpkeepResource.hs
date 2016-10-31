{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Crm.Server.Api.UpkeepResource (
  loadNextServiceTypeHint ,
  insertUpkeepMachines ,
  printDailyPlanListing' ,
  upkeepResource ) where

import           Opaleye.Operators           ((.==), (.===))
import           Opaleye.Manipulation        (runInsert, runUpdate, runDelete, runInsertReturning, runInsert)
import           Opaleye.PGTypes             (pgDay, pgBool, pgInt4, pgStrictText, PGInt4)
import           Opaleye                     (runQuery, Column, Nullable, maybeToNullable, showSqlForPostgres)

import           Database.PostgreSQL.Simple  (Connection)

import           Control.Monad.Reader        (ask)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Error.Class   (throwError)
import           Control.Monad               (forM_, forM, join)
import           Control.Monad.Trans.Except  (ExceptT)
import           Control.Arrow               (second)
import           Control.Lens                (view, over, mapped, _1, _3)

import           Data.Tuple.All              (sel2, sel3, sel4)
import           Data.List                   (nub)
import           Data.Text                   (pack, Text)
import           Data.Time.Calendar          (Day)
import           Data.Time.Clock             (getCurrentTime)
import           Data.Pool                   (withResource)

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
import qualified Crm.Shared.MachineKind      as MK
import qualified Crm.Shared.UpkeepMachine    as UM
import qualified Crm.Shared.UpkeepSequence   as US
import qualified Crm.Shared.ContactPerson    as CP
import qualified Crm.Shared.ServerRender     as SR
import qualified Crm.Shared.Company          as C
import qualified Crm.Shared.YearMonthDay     as YMD
import           Crm.Shared.MyMaybe

import           Crm.Server.Helpers          (prepareReaderTuple, createDeletion, createDeletion', catchError)
import           Crm.Server.Boilerplate      ()
import           Crm.Server.Types
import           Crm.Server.DB --
import           Crm.Server.Handler          (mkInputHandler', mkConstHandler', mkListing', deleteRows'', 
                                               mkGenHandler', mkDayParam, getDayParam)
import           Crm.Server.CachedCore       (recomputeWhole)
import           Crm.Server.Core             (nextServiceTypeHint)
import           Crm.Server.Parsers          (parseMarkup)
import qualified Crm.Server.Database.UpkeepMachine as UMD
import qualified Crm.Server.Database.UpkeepSequence as USD
import           Crm.Server.Database.MachineType

import           TupleTH                     (proj, catTuples, dropTuple, takeTuple)


data UpkeepsListing =
  UpkeepsAll |
  UpkeepsPlanned |
  PrintDailyPlan |
  UpkeepsCalled


addUpkeep :: 
  Connection -> 
  (U.Upkeep, [(UM.UpkeepMachine, M.MachineId)], [E.EmployeeId], MyMaybe U.UpkeepId) -> 
  IO U.UpkeepId -- ^ id of the upkeep
addUpkeep connection (upkeep', upkeepMachines, employeeIds, supertaskId') = do
  let superTaskId = maybeToNullable . fmap (pgInt4 . U.getUpkeepId) . toMaybe $ supertaskId'
  upkeepIds <- runInsertReturning
    connection
    upkeepsTable 
    (UpkeepRow
      (U.UpkeepId Nothing)
      (U.Upkeep
        (pgDay . YMD.ymdToDay . U.upkeepDate $ upkeep')
        (pgBool . U.upkeepClosed $ upkeep')
        (pgStrictText . U.workHours $ upkeep')
        (pgStrictText . U.workDescription $ upkeep')
        (pgStrictText . U.recommendation $ upkeep')
        (pgBool . U.setDate $ upkeep'))
      (U.UpkeepId superTaskId))
    (view upkeepPK)
  let upkeepId = head upkeepIds
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
      upkeepMachinesTable $
      UMD.UpkeepMachineRow {
        UMD._upkeepFK = pgInt4 `fmap` upkeepId ,
        UMD._machineFK = pgInt4 `fmap` upkeepMachineId ,
        UMD._upkeepMachine = UM.UpkeepMachine {
          UM.recordedMileage = pgInt4 . UM.recordedMileage $ upkeepMachine' , 
          UM.upkeepMachineNote = pgStrictText . UM.upkeepMachineNote $ upkeepMachine' ,
          UM.warrantyUpkeep = pgBool . UM.warrantyUpkeep $ upkeepMachine' ,
          UM.endNote = pgStrictText . UM.endNote $ upkeepMachine' ,
          UM.upkeepType = pgInt4 . UM.upkeepTypeEncode . UM.upkeepType $ upkeepMachine' }}
    return ()
  in forM_ upkeepMachines insertUpkeepMachine

removeUpkeep :: Handler (IdDependencies' U.UpkeepId)
removeUpkeep = mkConstHandler' jsonO $ do
  ((_, connection), U.UpkeepId upkeepIdInt) <- ask
  deleteRows'' [
    createDeletion upkeepEmployeesTable ,
    createDeletion' (U.getUpkeepId . view UMD.upkeepFK) upkeepMachinesTable , 
    createDeletion' (U.getUpkeepId . view upkeepPK) upkeepsTable ]
    upkeepIdInt connection

updateUpkeepHandler :: Handler (IdDependencies' U.UpkeepId)
updateUpkeepHandler = mkInputHandler' (jsonO . jsonI) $ \(upkeep', machines, employeeIds) -> let 
  upkeepTuple = (upkeep', machines)
  in do 
    ((cache, pool), upkeepId) <- ask
    withResource pool $ \connection -> liftIO $ 
      updateUpkeep connection upkeepId upkeepTuple employeeIds
    recomputeWhole pool cache

updateUpkeep :: 
  Connection -> 
  U.UpkeepId -> 
  (U.Upkeep, [(UM.UpkeepMachine, M.MachineId)]) -> 
  [E.EmployeeId] -> 
  IO ()
updateUpkeep conn upkeepId (upkeep', upkeepMachines) employeeIds = do
  _ <- let
    condition upkeepRow = view upkeepPK upkeepRow .=== fmap pgInt4 upkeepId
    readToWrite u =
      UpkeepRow 
        (Just `fmap` (view upkeepPK $ u))
        (U.Upkeep {
          U.upkeepDate = pgDay . YMD.ymdToDay . U.upkeepDate $ upkeep' ,
          U.upkeepClosed = pgBool . U.upkeepClosed $ upkeep' ,
          U.workHours = pgStrictText . U.workHours $ upkeep' ,
          U.workDescription = pgStrictText . U.workDescription $ upkeep' ,
          U.recommendation = pgStrictText . U.recommendation $ upkeep' ,
          U.setDate = pgBool . U.setDate $ upkeep' })
        (view upkeepSuper u)
    in runUpdate conn upkeepsTable readToWrite condition
  _ <- runDelete conn upkeepMachinesTable $ \upkeepRow -> UMD._upkeepFK upkeepRow .=== (pgInt4 `fmap` upkeepId)
  insertUpkeepMachines conn upkeepId upkeepMachines
  _ <- runDelete conn upkeepEmployeesTable $ \upkeepRow -> $(proj 3 0) upkeepRow .== (pgInt4 . U.getUpkeepId $ upkeepId)
  insertEmployees conn upkeepId employeeIds
  return ()

upkeepListing :: ListHandler Dependencies
upkeepListing = mkListing' jsonO $ const $ do
  (_, pool) <- ask 
  rows <- withResource pool $ \connection -> liftIO . over (mapped . mapped . _1 . upkeepSuper) (\x -> fmap (U.UpkeepId) (U.getUpkeepId x)) $ runQuery connection expandedUpkeepsQuery
  return . mapUpkeeps $ rows

upkeepsCalledListing :: ListHandler Dependencies
upkeepsCalledListing = mkListing' jsonO $ const $ do
  (_,pool) <- ask
  result <- upkeepsPlanned NormalTasks CalledUpkeep pool
  return $ (flip fmap) result $ \outer -> (flip fmap) outer $ \t ->
    $(catTuples 2 4) ($(takeTuple 7 2) t) ($(dropTuple 7 3) t) 

upkeepsPlannedListing :: ListHandler Dependencies
upkeepsPlannedListing = mkListing' jsonO $ const $ do
  (_,pool) <- ask
  res :: [UpkeepRow] <- over (mapped . mapped . upkeepSuper) (\x -> fmap (U.UpkeepId) (U.getUpkeepId x)) $ liftIO $ withResource pool $ \p -> runQuery p (upkeepQuery $ Just Subtasks)
  normal <- upkeepsPlanned NormalTasks ServiceUpkeep pool
  subtasks <- fmap join $ upkeepsPlanned Subtasks ServiceUpkeep pool

  let
    result = (flip fmap) normal $ \list -> let
      locateSubtasks tuple = over (mapped . _3) toMyMaybe (tuple : filter (\subtask -> (Just . $(proj 7 0) $ tuple) == $(proj 7 2) subtask) subtasks)
      in foldMap locateSubtasks list
  return result
  
upkeepsPlanned :: MonadIO m => DealWithSubtasks ->
  PlannedUpkeepType -> ConnectionPool ->
  m [[(U.UpkeepId, U.Upkeep, Maybe U.UpkeepId, C.CompanyId, C.Company,
        [(M.MachineId, Text, Text, MK.MachineKindEnum)],
        [EmployeeMapped])]]
upkeepsPlanned subtasks put pool = do
  rows <- liftIO $ withResource pool $ \connection -> over (mapped . mapped . _1 . upkeepSuper) (\x -> fmap (U.UpkeepId) (U.getUpkeepId x)) . runQuery connection $ groupedPlannedUpkeepsQuery' subtasks put

  (upkeeps' :: [(MK.MachineKindEnum, M.UpkeepBy, U.UpkeepId, U.Upkeep, Maybe U.UpkeepId, C.CompanyId, C.Company, 
      [(M.MachineId, Text, Text, MK.MachineKindEnum)])]) <- 
    liftIO $ forM rows $ \(u :: UpkeepRow, machineKind, machineUpkeepBy, (companyId :: C.CompanyId, company :: C.Company)) -> do
      notes <- withResource pool $ \connection -> runQuery connection (notesForUpkeep . view upkeepPK $ u)
      return (MK.dbReprToKind machineKind, M.upkeepByDecode machineUpkeepBy, view upkeepPK u, view upkeep u, view upkeepSuper u, companyId, 
        company, notes :: [(M.MachineId, Text, Text, MK.MachineKindEnum)])
  employeeRows <- liftIO $ withResource pool $ \connection -> runQuery connection 
    (employeesInUpkeeps (fmap $(proj 8 2) upkeeps'))
  let 
    (employees :: [(U.UpkeepId, EmployeeMapped)]) = (\(uId, e) -> (uId, convert e)) `fmap` employeeRows
    upkeepsWithEmployees = map (\tuple -> let
      employee = map snd . filter (($(proj 8 2) tuple ==) . fst) $ employees
      in $(catTuples 8 1) tuple employee) upkeeps'
    areWeDoingUpkeep (machineKind, machineUpkeepBy) = case machineUpkeepBy of
      M.UpkeepByDefault -> MK.isUpkeepByUs machineKind
      M.UpkeepByThem    -> False
      M.UpkeepByWe      -> True
    byUs = filter (areWeDoingUpkeep . $(takeTuple 9 2)) upkeepsWithEmployees
    byThem = filter (not . areWeDoingUpkeep . $(takeTuple 9 2)) upkeepsWithEmployees
  let toReturn = [fmap $(dropTuple 9 2) byUs, fmap $(dropTuple 9 2) byThem]
  return toReturn

    
upkeepCompanyMachines :: Handler (IdDependencies' U.UpkeepId)
upkeepCompanyMachines = mkConstHandler' jsonO $ do
  ((_, pool), upkeepId) <- ask
  upkeeps <- withResource pool $ \connection -> liftIO $ 
    fmap (mapUpkeeps . over (mapped . _1 . upkeepSuper) (\x -> fmap (U.UpkeepId) (U.getUpkeepId x))) $ runQuery connection $ expandedUpkeepsQuery2 upkeepId
  upkeep' <- singleRowOrColumn upkeeps
  allMachines <- withResource pool $ \connection -> liftIO $ runQuery connection (machinesInCompanyQuery' upkeepId)
  employeeIds <- withResource pool $ \connection -> liftIO $ runQuery connection (employeeIdsInUpkeep upkeepId)
  let machines = fmap (\(m, mt) -> (m :: MachineRecord, mt :: MachineTypeRecord)) allMachines
  machines'' <- withResource pool $ \connection -> loadNextServiceTypeHint machines connection
  companyId <- case machines of
    [] -> throwError NotAllowed
    (machineMapped,_) : _ -> return . _companyFK $ machineMapped

  return (companyId, (toMyMaybe . sel2 $ upkeep', sel3 upkeep', sel4 upkeep', fmap E.EmployeeId employeeIds), map
    (\(m, mt, nextUpkeepSequence) -> (_machinePK m, _machine m, $(proj 2 1) mt, nextUpkeepSequence)) machines'')

loadNextServiceTypeHint :: 
  (MonadIO m) => 
  [(MachineRecord, MachineTypeRecord)] -> 
  Connection -> 
  ExceptT (Reason Text) m [(MachineRecord, (MT.MachineTypeId, MT.MachineType), US.UpkeepSequence)]
loadNextServiceTypeHint machines conn = forM machines $ \(machine', machineType') -> do
  upkeepSequences' <- liftIO $ runQuery conn $ upkeepSequencesByIdQuery $ _machineTypePK machineType'
  pastUpkeepMachines' <- liftIO $ runQuery conn (pastUpkeepMachinesQ (_machinePK machine'))
  let upkeepSequences = fmap (\(x :: USD.UpkeepSequenceRecord) -> USD._upkeepSequence x) upkeepSequences'
  let pastUpkeepMachines = fmap (view UMD.upkeepMachine) (pastUpkeepMachines' :: [UMD.UpkeepMachineRow])
  uss <- case upkeepSequences of
    (us' : uss') -> return (us', uss')
    _ -> throwError . CustomReason . DomainReason . pack $ "Db in invalid state"
  return (machine', (_machineTypePK machineType', _machineType machineType'), nextServiceTypeHint uss pastUpkeepMachines)

printDailyPlanListing' ::   
  (MonadIO m, Functor m) => 
  Maybe E.EmployeeId -> 
  Connection -> 
  Day -> 
  ExceptT (Reason r) m [(U.UpkeepMarkup, C.Company, [(E.EmployeeId, E.Employee)], [(M.Machine, MT.MachineType, MyMaybe CP.ContactPerson, (UM.UpkeepMachine, MyMaybe [SR.Markup]))])]
printDailyPlanListing' employeeId connection day = do
  dailyPlanUpkeeps' :: [(UpkeepRow, [Int])] <- liftIO . fmap (over (mapped . _1 . upkeepSuper) (\x -> fmap (U.UpkeepId) (U.getUpkeepId x))) $ runQuery connection (dailyPlanQuery employeeId day)
  dailyPlanUpkeeps <- forM dailyPlanUpkeeps' $ \(upkeep', es) -> do
    let 
      listifyNote (um @ (UM.UpkeepMachine {})) = 
        (um, catchError . parseMarkup . UM.upkeepMachineNote $ um) where
      upkeepRaw = view upkeep upkeep'
      upkeepMarkup = upkeepRaw {
        U.workDescription = maybe ((:[]) . SR.PlainText . U.workDescription $ upkeepRaw) (\x -> x) . 
          catchError . parseMarkup . U.workDescription $ upkeepRaw }
    employees <- fmap (map $ \e -> let 
      employee = (convert e :: EmployeeMapped) 
      in ($(proj 2 0) employee, $(proj 2 1) employee)) $ 
      liftIO $ runQuery connection (multiEmployeeQuery es)
    machines <- fmap (map $ \(m :: MachineRecord, mt :: MachineTypeRecord, cp, um :: UMD.UpkeepMachineRow) -> (
      _machine m , 
      _machineType mt ,
      toMyMaybe . $(proj 3 2) $ (convert cp :: MaybeContactPersonMapped) ,
      second toMyMaybe . listifyNote . view UMD.upkeepMachine $ um)) $ 
      liftIO . runQuery connection . machinesInUpkeepQuery'' . view upkeepPK $ upkeep'
    (company :: C.Company) <-
      singleRowOrColumn =<< (liftIO $ runQuery connection (companyInUpkeepQuery . view upkeepPK $ upkeep'))
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
    UpkeepsCalled -> upkeepsCalledListing
    PrintDailyPlan -> printDailyPlanListing ,
  name = A.upkeep ,
  update = Just updateUpkeepHandler ,
  schema = upkeepSchema ,
  remove = Just removeUpkeep ,
  create = Just createUpkeepHandler ,
  get = Just upkeepCompanyMachines }

upkeepSchema :: S.Schema U.UpkeepId UpkeepsListing Void
upkeepSchema = S.withListing UpkeepsAll (S.named [
  ("called", S.listing UpkeepsCalled) ,
  (A.planned, S.listing UpkeepsPlanned) ,
  ("print", S.listing PrintDailyPlan) ,
  (A.single, S.singleRead id )])
