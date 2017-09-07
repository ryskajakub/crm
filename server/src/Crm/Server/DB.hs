{-# OPTIONS -fno-warn-missing-signatures #-}

{-# LANGUAGE Arrows                 #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module Crm.Server.DB (
  -- tables
  companiesTable ,
  machinesTable ,
  upkeepsTable ,
  upkeepMachinesTable ,
  employeesTable ,
  upkeepSequencesTable ,
  machinePhotosTable ,
  photosMetaTable ,
  contactPersonsTable ,
  extraFieldSettingsTable ,
  extraFieldsTable ,
  passwordTable ,
  readonlyPasswordTable ,
  upkeepEmployeesTable ,
  taskEmployeesTable ,
  tasksTable ,
  upkeepPhotosTable ,
  machineTypePhotosTable ,
  -- basic queries
  extraFieldSettingsQuery ,
  extraFieldsQuery ,
  upkeepMachinesQuery ,
  employeesQuery ,
  machinePhotosQuery ,
  getPhoto ,
  singleEmployeeQuery ,
  contactPersonsQuery ,
  upkeepQuery ,
  -- manipulations
  addPhoto ,
  deletePhoto ,
  updatePhoto ,
  -- runs
  runExpandedMachinesQuery ,
  runMachinesInCompanyQuery ,
  runMachineTypesQuery' ,
  runMachinesInCompanyByUpkeepQuery ,
  runCompanyUpkeepsQuery ,
  -- more complex query
  expandedMachinesQuery ,
  machinesQ ,
  extraFieldsForMachineQuery ,
  machineIdsHavingKind ,
  extraFieldsPerKindQuery ,
  otherMachinesInCompanyQuery ,
  expandedUpkeepsQuery2 ,
  groupedPlannedUpkeepsQuery ,
  groupedPlannedUpkeepsQuery' ,
  expandedUpkeepsQuery ,
  companyByIdQuery ,
  companyByIdCompanyQuery ,
  machineDetailQuery ,
  contactPersonsByIdQuery ,
  nextServiceMachinesQuery ,
  nextServiceUpkeepsQuery ,
  nextServiceUpkeepSequencesQuery ,
  upkeepsDataForMachine ,
  expandedUpkeepsByCompanyQuery ,
  machineTypesWithCountQuery ,
  upkeepSequencesByIdQuery ,
  singleMachineTypeQuery ,
  singleContactPersonQuery ,
  machinesInUpkeepQuery ,
  machinePhotosByMachineId ,
  machineTypePhotosByMachineTypeId ,
  photoMetaQuery ,
  machineManufacturersQuery ,
  employeesInUpkeep ,
  employeeIdsInUpkeep ,
  notesForUpkeep ,
  machinesInUpkeepQuery'' ,
  pastUpkeepMachinesQ ,
  dailyPlanQuery ,
  multiEmployeeQuery ,
  companyInUpkeepQuery ,
  mainEmployeesInDayQ ,
  employeesInUpkeeps ,
  tasksForEmployeeQuery ,
  getTaskQuery ,
  lastRecommendationQuery ,
  machinesInCompanyQuery' ,
  takenColoursQuery ,
  photosInUpkeepQuery ,
  upkeepForPhotoQ ,
  machinesForTypeQ ,
  -- manipulations
  insertExtraFields ,
  -- helpers
  withConnection ,
  singleRowOrColumn ,
  mapUpkeeps ,
  initiateConnection ,
  -- tables
  PasswordTable ,
  MachineRow'' (..), machine, machinePK ,
  -- mappings
  MachineRecord ,
  ColumnToRecordDeep ,
  convert ,
  convertDeep ,
  MaybeContactPersonMapped ,
  MachineTypeMapped ,
  ContactPersonMapped ,
  CompanyMapped ,
  MaybeEmployeeMapped ,
  EmployeeMapped ,
  TaskMapped ,
  UpkeepSequenceMapped ,
  UpkeepMachineMapped ,
  PhotoMetaMapped ,
  ExtraFieldSettingsMapped ,
  ExtraFieldMapped ,
  MachineMapped ,
  UpkeepRow, UpkeepRow'' (..), upkeep, upkeepPK, upkeepSuper,
  CompanyRecord, companyPK, companyCoords, companyCore, CompanyTable' (..) ,
  -- types
  PlannedUpkeepType (..) ,
  DealWithSubtasks (..) ,
  PhotosMetaTable
  ) where


import           Prelude                              hiding (not)

import           Control.Arrow                        (arr, returnA, (<<<),
                                                       (^<<))
import           Control.Lens                         (Getting, makeLenses,
                                                       mapped, over, view, _1)
import           Control.Monad                        (forM_)
import           Data.List                            (intersperse, nubBy)
import           Data.Monoid                          ((<>))

import           Control.Monad.Error.Class            (throwError)
import           Control.Monad.IO.Class               (MonadIO, liftIO)
import           Control.Monad.Trans.Except           (ExceptT)
import           Data.ByteString.Lazy                 (ByteString)
import           Data.Profunctor.Product              (p1, p2, p3, p4, p5)
import           Data.Profunctor.Product.Default      (Default)
import           Data.Profunctor.Product.TH           (makeAdaptorAndInstance')
import           Data.Text                            (Text, pack)
import           Data.Time.Calendar                   (Day)
import           Data.Tuple.All                       (Sel1, sel1, sel2, sel3,
                                                       sel4, sel5, sel6, sel7,
                                                       uncurryN, upd2, upd4)
import           Database.PostgreSQL.Simple           (Binary (..),
                                                       ConnectInfo (..),
                                                       Connection, Only (..),
                                                       close, connect,
                                                       defaultConnectInfo,
                                                       execute, query)
import           Opaleye                              (runInsert)
import qualified Opaleye.Aggregate                    as AGG
import           Opaleye.Column                       (Column, Nullable, isNull)
import           Opaleye.Distinct                     (distinct)
import qualified Opaleye.Internal.Aggregate           as IAGG
import qualified Opaleye.Internal.Column              as C
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ
import           Opaleye.Internal.Operators           (EqPP)
import           Opaleye.Internal.TableMaker          (ColumnMaker)
import           Opaleye.Join                         (leftJoin)
import           Opaleye.Operators                    (in_, lower, not,
                                                       restrict, (.==), (.===))
import           Opaleye.Order                        (PGOrd, asc, desc, limit,
                                                       orderBy)
import           Opaleye.PGTypes                      (PGArray, PGBool, PGBytea,
                                                       PGDate, PGFloat8, PGInt4,
                                                       PGText, pgBool, pgDay,
                                                       pgInt4, pgStrictText,
                                                       pgString)
import           Opaleye.QueryArr                     (Query, QueryArr)
import           Opaleye.RunQuery                     (runQuery)
import           Opaleye.Table                        (Table (Table), optional,
                                                       queryTable, required)

import           Rest.Types.Error                     (DataError (ParseError),
                                                       Reason (InputError))

import qualified Crm.Shared.Company                   as C
import qualified Crm.Shared.ContactPerson             as CP
import qualified Crm.Shared.Employee                  as E
import qualified Crm.Shared.ExtraField                as EF
import qualified Crm.Shared.Machine                   as M
import qualified Crm.Shared.MachineKind               as MK
import qualified Crm.Shared.MachineType               as MT
import qualified Crm.Shared.Photo                     as P
import qualified Crm.Shared.PhotoMeta                 as PM
import qualified Crm.Shared.Task                      as T
import qualified Crm.Shared.Upkeep                    as U
import qualified Crm.Shared.UpkeepMachine             as UM
import qualified Crm.Shared.UpkeepSequence            as US
import qualified Crm.Shared.YearMonthDay              as YMD

import           Crm.Server.Helpers                   (maybeToNullable)
import           Crm.Server.Types

import           Crm.Server.Database.MachineType
import qualified Crm.Server.Database.MachineType      as MTD
import           Crm.Server.Database.PrimaryKeys
import           Crm.Server.Database.Types
import           Crm.Server.Database.UpkeepMachine    as UMD
import           Crm.Server.Database.UpkeepSequence   hiding (machineTypeFK,
                                                       _machineTypeFK)
import qualified Crm.Server.Database.UpkeepSequence   as USD

type ContactPersonsTable = (DBInt, DBInt, DBText, DBText, DBText)
type ContactPersonsLeftJoinTable = (Column (Nullable PGInt4), Column (Nullable PGInt4),
  Column (Nullable PGText), Column (Nullable PGText), Column (Nullable PGText))
type ContactPersonsWriteTable = (Maybe DBInt, DBInt, DBText, DBText, DBText)

type EmployeeTable = (DBInt, DBText, DBText, DBText, DBText)
type EmployeeLeftJoinTable = (Column (Nullable PGInt4), Column (Nullable PGText), Column (Nullable PGText),
  Column (Nullable PGText), Column (Nullable PGText))
type EmployeeWriteTable = (Maybe DBInt, DBText, DBText, DBText, DBText)

type PhotosMetaTable = (DBInt, DBText, DBText)

type MachinePhotosTable = (DBInt, DBInt)
type MachineTypePhotosTable = (DBInt, DBInt)
type UpkeepPhotosTable = (DBInt, DBInt)

type ExtraFieldSettingsTable = (DBInt, DBInt, DBInt, DBText)
type ExtraFieldSettingsWriteTable = (Maybe DBInt, DBInt, DBInt, DBText)

type ExtraFieldsTable = (DBInt, DBInt, DBText)

type PasswordTable = (Column PGBytea)

type UpkeepEmployeesTable = (DBInt, DBInt, DBInt)

type TaskEmployeesTable = (DBInt, DBInt)
type TasksWriteTable = (Maybe DBInt, DBDate, DBText, Column (Nullable PGDate))
type TasksTable = (DBInt, DBDate, DBText, Column (Nullable PGDate))

passwordTable :: Table PasswordTable PasswordTable
passwordTable = Table "password" $ p1 ( required "password" )

readonlyPasswordTable :: Table PasswordTable PasswordTable
readonlyPasswordTable = Table "readonly_password" $ p1 ( required "password" )

extraFieldsTable :: Table ExtraFieldsTable ExtraFieldsTable
extraFieldsTable = Table "extra_fields" $ p3 (
  required "extra_field_id" ,
  required "machine_id" ,
  required "value" )

extraFieldSettingsTable :: Table ExtraFieldSettingsWriteTable ExtraFieldSettingsTable
extraFieldSettingsTable = Table "extra_field_settings" $ p4 (
  optional "id" ,
  required "kind" ,
  required "order_" ,
  required "name" )

photosMetaTable :: Table PhotosMetaTable PhotosMetaTable
photosMetaTable = Table "photos_meta" $ p3 (
  required "photo_id" ,
  required "mime_type" ,
  required "file_name" )

machinePhotosTable :: Table MachinePhotosTable MachinePhotosTable
machinePhotosTable = Table "machine_photos" $ p2 (
  required "photo_id" ,
  required "machine_id" )

machineTypePhotosTable :: Table MachineTypePhotosTable MachineTypePhotosTable
machineTypePhotosTable = Table "machine_type_photos" $ p2 (
  required "photo_id" ,
  required "machine_type_id" )

upkeepPhotosTable :: Table UpkeepPhotosTable UpkeepPhotosTable
upkeepPhotosTable = Table "upkeep_photos" $ p2 (
  required "photo_id" ,
  required "upkeep_id" )


data CompanyTable' companyPK companyCore companyCoords = CompanyTable {
  _companyPK     :: companyPK ,
  _companyCore   :: companyCore ,
  _companyCoords :: companyCoords }
makeLenses ''CompanyTable'

type CompanyCore = C.Company' DBText DBText DBText DBBool
type CompanyCoords = C.Coordinates' (Column (Nullable PGFloat8)) (Column (Nullable PGFloat8))
type CompanyRead = CompanyTable' (C.CompanyId' DBInt) CompanyCore CompanyCoords
type CompanyWrite = CompanyTable' (C.CompanyId' (Maybe DBInt)) CompanyCore CompanyCoords
type CompanyRecord = CompanyTable' C.CompanyId C.Company (Maybe C.Coordinates)

makeAdaptorAndInstance' ''CompanyTable'

companiesTable :: Table CompanyWrite CompanyRead
companiesTable = Table "companies" $ pCompanyTable CompanyTable {
  _companyPK = (C.pCompanyId C.CompanyId {
    C.getCompanyId = optional "id" }) ,
  _companyCore = (C.pCompany C.Company {
    C.companyName = required "name" ,
    C.companyNote = required "note" ,
    C.companyAddress = required "address" ,
    C.smallCompany = required "small_company" }) ,
  _companyCoords = (C.pCoordinates C.Coordinates {
    C.latitude = required "latitude" ,
    C.longitude = required "longitude" })}


data MachineRow'' machinePK companyId contactPersonId
    machineTypeId linkageId machine = MachineRow {
  _machinePK       :: machinePK ,
  _companyFK       :: companyId ,
  _contactPersonFK :: contactPersonId ,
  _machineTypeFK   :: machineTypeId ,
  _linkageFK       :: linkageId ,
  _machine         :: machine }
makeLenses ''MachineRow''

type MachineRow' machinePK = MachineRow''
  machinePK
    (C.CompanyId' DBInt) (Column (Nullable PGInt4)) MachineTypePK (M.MachineId' (Column (Nullable PGInt4)))
  (M.Machine' (Column (Nullable PGDate)) DBInt DBInt DBText DBText DBText DBBool DBText DBInt)
type MachinesTable = MachineRow' MachinePK
type MachinesWrite = MachineRow' (M.MachineId' (Maybe DBInt))
type MachineRecord = MachineRow'' M.MachineId C.CompanyId (Maybe Int) MT.MachineTypeId (M.MachineId' (Maybe Int))
  M.Machine

makeAdaptorAndInstance' ''MachineRow''

machinesTable :: Table MachinesWrite MachinesTable
machinesTable = Table "machines" $ pMachineRow MachineRow {
  _machinePK = (M.pMachineId M.MachineId {
    M.getMachineId = optional "id" }) ,
  _companyFK = (C.pCompanyId C.CompanyId {
    C.getCompanyId = required "company_id" }) ,
  _contactPersonFK = required "contact_person_id" ,
  _machineTypeFK = MT.pMachineTypeId . MT.MachineTypeId . required $ "machine_type_id" ,
  _linkageFK = (M.pMachineId M.MachineId {
    M.getMachineId = required "linkage_id" }) ,
  _machine = (M.pMachine M.Machine {
    M.machineOperationStartDate = required "operation_start" ,
    M.initialMileage = required "initial_mileage" ,
    M.mileagePerYear = required "mileage_per_year" ,
    M.label_ = required "label" ,
    M.serialNumber = required "serial_number" ,
    M.yearOfManufacture = required "year_of_manufacture" ,
    M.archived = required "archived" ,
    M.furtherSpecification = required "note" ,
    M.upkeepBy = required "upkeep_by" })}


contactPersonsTable :: Table ContactPersonsWriteTable ContactPersonsTable
contactPersonsTable = Table "contact_persons" $ p5 (
  optional "id" ,
  required "company_id" ,
  required "name" ,
  required "phone" ,
  required "position" )


data UpkeepRow'' upkeepPK upkeep upkeepSuper = UpkeepRow {
  _upkeepPK    :: upkeepPK ,
  _upkeep      :: upkeep ,
  _upkeepSuper :: upkeepSuper }
  deriving Show
makeLenses ''UpkeepRow''

type UpkeepRow' upkeepPK upkeepSuper = UpkeepRow'' upkeepPK
  (U.UpkeepGen'' DBDate DBBool DBText DBBool DBText DBText) upkeepSuper
type UpkeepsTable = UpkeepRow' UpkeepPK (U.UpkeepId' (Column (Nullable PGInt4)))
type UpkeepsWriteTable = UpkeepRow' (U.UpkeepId' (Maybe DBInt)) (U.UpkeepId' (Column (Nullable PGInt4)))
type UpkeepsLeftJoinTable = UpkeepRow''
  (U.UpkeepId' MBInt)
  (U.UpkeepGen'' MBDate MBBool MBText MBBool MBText MBText)
  (U.UpkeepId' (Column (Nullable PGInt4)))
type UpkeepRow = UpkeepRow'' U.UpkeepId U.Upkeep (Maybe U.UpkeepId)

mapMaybeUpkeep ::
  UpkeepRow''
    (U.UpkeepId' (Maybe Int))
    (U.UpkeepGen'' (Maybe Day) (Maybe Bool) (Maybe Text) (Maybe Bool) (Maybe Text) (Maybe Text))
    (U.UpkeepId' (Maybe Int)) ->
  Maybe UpkeepRow
mapMaybeUpkeep (UpkeepRow uId' u' suId') = let
  uId = fmap U.UpkeepId (U.getUpkeepId uId')
  u = pure U.Upkeep <*> fmap YMD.dayToYmd (U.upkeepDate u') <*> U.upkeepClosed u' <*>
    U.workHours u' <*> U.workDescription u' <*> U.recommendation u' <*> U.setDate u'
  suId = fmap U.UpkeepId (U.getUpkeepId suId')
  in pure UpkeepRow <*> uId <*> u <*> Just suId

makeAdaptorAndInstance' ''UpkeepRow''

upkeepsTable :: Table UpkeepsWriteTable UpkeepsTable
upkeepsTable = Table "upkeeps" $ (pUpkeepRow UpkeepRow {
  _upkeepSuper = U.pUpkeepId ( U.UpkeepId . required $ "supertask_id" ) ,
  _upkeepPK = U.pUpkeepId ( U.UpkeepId . optional $ "id" ) ,
  _upkeep = U.pUpkeep U.Upkeep {
    U.upkeepDate = required "date_" ,
    U.upkeepClosed = required "closed" ,
    U.workHours = required "work_hours" ,
    U.workDescription = required "work_description" ,
    U.recommendation = required "recommendation" ,
    U.setDate = required "set_date" }})


employeesTable :: Table EmployeeWriteTable EmployeeTable
employeesTable = Table "employees" $ p5 (
  optional "id" ,
  required "name" ,
  required "contact" ,
  required "capabilities" ,
  required "colour" )

upkeepEmployeesTable :: Table UpkeepEmployeesTable UpkeepEmployeesTable
upkeepEmployeesTable = Table "upkeep_employees" $ p3 (
  required "upkeep_id" ,
  required "employee_id" ,
  required "order_" )

taskEmployeesTable :: Table TaskEmployeesTable TaskEmployeesTable
taskEmployeesTable = Table "task_employees" $ p2 (
  required "task_id" ,
  required "employee_id" )

tasksTable :: Table TasksWriteTable TasksTable
tasksTable = Table "tasks" $ p4 (
  optional "id" ,
  required "start_date" ,
  required "description" ,
  required "end_date" )

extraFieldsQuery :: Query ExtraFieldsTable
extraFieldsQuery = queryTable extraFieldsTable

extraFieldSettingsQuery :: Query ExtraFieldSettingsTable
extraFieldSettingsQuery = queryTable extraFieldSettingsTable

contactPersonsQuery :: Query ContactPersonsTable
contactPersonsQuery = queryTable contactPersonsTable

photosMetaQuery :: Query PhotosMetaTable
photosMetaQuery = queryTable photosMetaTable

machinePhotosQuery :: Query MachinePhotosTable
machinePhotosQuery = queryTable machinePhotosTable

machineTypePhotosQuery :: Query MachineTypePhotosTable
machineTypePhotosQuery = queryTable machineTypePhotosTable

upkeepMachinesQuery :: Query UpkeepMachinesTable
upkeepMachinesQuery = queryTable upkeepMachinesTable

employeesQuery :: Query EmployeeTable
employeesQuery = queryTable employeesTable

class ColumnToRecordDeep tupleIn tupleOut | tupleOut -> tupleIn where
  convertDeep :: tupleIn -> tupleOut

instance (ColumnToRecord c1 r1, ColumnToRecord c2 r2) => ColumnToRecordDeep (c1,c2) (r1,r2) where
  convertDeep (c1, c2) = (convert c1, convert c2)
instance (ColumnToRecord c1 r1, ColumnToRecord c2 r2, ColumnToRecord c3 r3) =>
    ColumnToRecordDeep (c1,c2,c3) (r1,r2,r3) where
  convertDeep (c1, c2, c3) = (convert c1, convert c2, convert c3)

class ColumnToRecord column record | record -> column where
  convert :: column -> record

type MachineMapped = (M.MachineId, C.CompanyId, Maybe CP.ContactPersonId, MT.MachineTypeId, Maybe M.MachineId, M.Machine)
type CompanyMapped = (C.CompanyId, C.Company, Maybe C.Coordinates)
type MachineTypeMapped = (MT.MachineTypeId, MT.MachineType)
type ContactPersonMapped = (CP.ContactPersonId, C.CompanyId, CP.ContactPerson)
type MaybeContactPersonMapped = (Maybe CP.ContactPersonId, Maybe C.CompanyId, Maybe CP.ContactPerson)
type MaybeEmployeeMapped = (Maybe E.EmployeeId, Maybe E.Employee)
type EmployeeMapped = (E.EmployeeId, E.Employee)
type UpkeepSequenceMapped = (MT.MachineTypeId, US.UpkeepSequence)
type UpkeepMachineMapped = (U.UpkeepId, M.MachineId, UM.UpkeepMachine)
type PhotoMetaMapped = (P.PhotoId, PM.PhotoMeta)
type ExtraFieldSettingsMapped = (EF.ExtraFieldId, MK.MachineKindSpecific)
type ExtraFieldMapped = (EF.ExtraFieldId, M.MachineId, Text)
type TaskMapped = (T.TaskId, T.Task)

instance ColumnToRecord (Int, Text, Text, Text, Bool, Maybe Double, Maybe Double) CompanyMapped where
  convert tuple = let
    company = (uncurryN $ const ((fmap . fmap . fmap . fmap) (const . const) C.Company)) tuple
    coordinates = pure C.Coordinates <*> sel6 tuple <*> sel7 tuple
    in (C.CompanyId $ sel1 tuple, company, coordinates)
instance ColumnToRecord (Int, Int, Text, Text) MachineTypeMapped where
  convert tuple = (MT.MachineTypeId $ sel1 tuple, (uncurryN $ const MT.MachineType)
    (upd2 (MK.dbReprToKind $ sel2 tuple) tuple))
instance ColumnToRecord (Int, Int, Text, Text, Text) ContactPersonMapped where
  convert tuple = (CP.ContactPersonId $ sel1 tuple, C.CompanyId $ sel2 tuple,
    (uncurryN $ const $ const CP.ContactPerson) tuple)
instance ColumnToRecord (Maybe Int, Maybe Int, Maybe Text, Maybe Text, Maybe Text) MaybeContactPersonMapped where
  convert tuple = let
    maybeCp = pure CP.ContactPerson <*> sel3 tuple <*> sel4 tuple <*> sel5 tuple
    in (CP.ContactPersonId `fmap` sel1 tuple, C.CompanyId `fmap` sel2 tuple, maybeCp)
instance ColumnToRecord (Int, Text, Text, Text, Text) EmployeeMapped where
  convert tuple = (E.EmployeeId $ sel1 tuple, uncurryN (const E.Employee) $ tuple)
instance ColumnToRecord (Maybe Int, Maybe Text, Maybe Text, Maybe Text, Maybe Text) MaybeEmployeeMapped where
  convert tuple = let
    maybeE = pure E.Employee <*> sel2 tuple <*> sel3 tuple <*> sel4 tuple <*> sel5 tuple
    in (E.EmployeeId `fmap` sel1 tuple, maybeE)
instance ColumnToRecord (Int, Text, Int, Int, Bool) UpkeepSequenceMapped where
  convert (a,b,c,d,e) = (MT.MachineTypeId d, US.UpkeepSequence a b c e)
instance ColumnToRecord (Int, Text, Int, Int, Bool, Text, Int) UpkeepMachineMapped where
  convert (a,b,c,d,e,f,g) = (U.UpkeepId a, M.MachineId c, UM.UpkeepMachine b d e f (UM.upkeepTypeDecode g))
instance ColumnToRecord (Int, Text, Text) PhotoMetaMapped where
  convert tuple = (P.PhotoId $ sel1 tuple, (uncurryN $ const PM.PhotoMeta) tuple PM.Unknown)
instance ColumnToRecord (Int, Int, Int, Text) ExtraFieldSettingsMapped where
  convert row = (EF.ExtraFieldId $ sel1 row, MK.MachineKindSpecific $ sel4 row)
instance ColumnToRecord (Int, Int, Text) ExtraFieldMapped where
  convert tuple = (EF.ExtraFieldId $ sel1 tuple, M.MachineId $ sel2 tuple, sel3 tuple)
instance ColumnToRecord (Int, Day, Text, Maybe Day) TaskMapped where
  convert tuple = let
    (id', startDate, description, endDate) = tuple
    in (T.TaskId id', T.Task (YMD.dayToYmd startDate) description (YMD.dayToYmd `fmap` endDate))

instance (ColumnToRecord a b) => ColumnToRecord [a] [b] where
  convert rows = fmap convert rows

-- todo rather do two queries
mapUpkeeps ::
  [(UpkeepRow, UMD.UpkeepMachineRow)] ->
  [(U.UpkeepId, Maybe U.UpkeepId, U.Upkeep, [(UM.UpkeepMachine, M.MachineId)])]
mapUpkeeps rows = foldl (\acc (upkeepCols, upkeepMachineCols) ->
  let
    upkeepMachineToAdd = (view UMD.upkeepMachine upkeepMachineCols, view UMD.machineFK upkeepMachineCols)
    addUpkeep' = (_upkeepPK upkeepCols, _upkeepSuper upkeepCols, _upkeep upkeepCols, [upkeepMachineToAdd])
    in case acc of
      [] -> [addUpkeep']
      row : rest | sel1 row == sel1 addUpkeep' ->
        upd4 (upkeepMachineToAdd : sel4 row) row : rest
      _ -> addUpkeep' : acc
  ) [] rows

-- | joins table according with the id in
join ::
  (Sel1 a DBInt) =>
  Query a ->
  QueryArr DBInt a
join tableQuery = proc id' -> do
  table <- tableQuery -< ()
  restrict -< sel1 table .== id'
  returnA -< table

upkeepQuery :: Maybe DealWithSubtasks -> Query UpkeepsTable
upkeepQuery dealWithSubtasks = let
  modifier = (flip fmap) dealWithSubtasks $ \x -> (if x == Subtasks then not else id) . isNull
  in proc () -> do
    rows <- queryTable upkeepsTable -< ()
    restrict -< case modifier of
      Just modifier' -> modifier' . U.getUpkeepId . _upkeepSuper $ rows
      Nothing        -> pgBool True
    returnA -< rows

-- | query upkeep table of joins
superUpkeepQuery :: Query UpkeepsTable
superUpkeepQuery = upkeepQuery $ Just NormalTasks

machinePhotosByMachineId :: M.MachineId -> Query PhotosMetaTable
machinePhotosByMachineId (M.MachineId machineId) =
  photosById machineId machinePhotosQuery

machineTypePhotosByMachineTypeId :: MT.MachineTypeId -> Query PhotosMetaTable
machineTypePhotosByMachineTypeId (MT.MachineTypeId machineTypeId) =
  photosById machineTypeId machineTypePhotosQuery

photosById :: Int -> Query (DBInt, DBInt) -> Query PhotosMetaTable
photosById int query' = proc () -> do
  (photoId, dbInt) <- query' -< ()
  restrict -< (dbInt .== pgInt4 int)
  (_, mimeType, fileName) <- join photosMetaQuery -< photoId
  returnA -< (photoId, mimeType, fileName)

machineManufacturersQuery :: String -> Query DBText
machineManufacturersQuery str = autocomplete $ distinct $ proc () -> do
  machineTypeRow <- queryTable MTD.machineTypesTable -< ()
  restrict -< lower (MT.machineTypeManufacturer . _machineType $ machineTypeRow) `like`
    (lower $ pgStrictText ("%" <> (pack $ intersperse '%' str) <> "%"))
  returnA -< MT.machineTypeManufacturer . _machineType $ machineTypeRow

otherMachinesInCompanyQuery :: C.CompanyId -> Query MachinesTable
otherMachinesInCompanyQuery companyId =
  joinL machinesTable companyFK <<< arr (const $ fmap pgInt4 companyId)

photoMetaQuery :: Int -> Query PhotosMetaTable
photoMetaQuery photoId = proc () -> do
  result <- join photosMetaQuery -< pgInt4 photoId
  returnA -< result

upkeepSequencesByIdQuery' :: QueryArr MachineTypePK UpkeepSequencesTable
upkeepSequencesByIdQuery' =
  joinL USD.upkeepSequencesTable USD.machineTypeFK

upkeepSequencesByIdQuery :: MT.MachineTypeId -> Query UpkeepSequencesTable
upkeepSequencesByIdQuery machineTypeId' = proc () -> do
  upkeepSequenceRow' <- upkeepSequencesByIdQuery' -< fmap pgInt4 machineTypeId'
  returnA -< upkeepSequenceRow'

pastUpkeepMachinesQ :: M.MachineId -> Query UpkeepMachinesTable
pastUpkeepMachinesQ machineId = proc () -> do
  upkeepMachineRow <- joinL upkeepMachinesTable UMD.machineFK -< fmap pgInt4 machineId
  upkeepRow <- joinQ superUpkeepQuery upkeepPK -< UMD._upkeepFK upkeepMachineRow
  restrict -< U.upkeepClosed . _upkeep $ upkeepRow
  returnA -< upkeepMachineRow

like :: Column PGText -> Column PGText -> Column PGBool
like = C.binOp HPQ.OpLike

autocomplete :: (PGOrd a) => Query (Column a) -> Query (Column a)
autocomplete = limit 10 . orderBy (asc id)

machineTypesQuery' :: String -> Query DBText
machineTypesQuery' mid = autocomplete $ proc () -> do
  machineTypeRow <- queryTable MTD.machineTypesTable -< ()
  restrict -< lower (MT.machineTypeName . _machineType $ machineTypeRow) `like`
    (lower $ pgStrictText ("%" <> (pack $ intersperse '%' mid) <> "%"))
  returnA -< MT.machineTypeName . _machineType $ machineTypeRow

companyByIdQuery :: C.CompanyId -> Query CompanyRead
companyByIdQuery companyId = proc () -> do
  companyRow <- queryTable companiesTable -< ()
  restrict -< (C.getCompanyId (_companyPK companyRow :: C.CompanyId' DBInt))
    .== (C.getCompanyId (fmap pgInt4 companyId :: C.CompanyId' DBInt))
  returnA -< companyRow

companyByIdCompanyQuery :: C.CompanyId -> Query CompanyCore
companyByIdCompanyQuery companyId = _companyCore ^<< companyByIdQuery companyId

machineTypesWithCountQuery :: Query (MachineTypesTable, DBInt8)
machineTypesWithCountQuery = let
  machineTypeIdQ = proc () -> do
    machineRow <- queryTable machinesTable -< ()
    returnA -< view machineTypeFK machineRow
  query' :: Query ( MachineTypesTable, MT.MachineTypeId' (Column (Nullable PGInt4)))
  query' = leftJoin (queryTable MTD.machineTypesTable) machineTypeIdQ $
    \(mtRow, mtId) -> MTD._machineTypePK mtRow .=== mtId
  mkAggregatedQuery = AGG.aggregate $ p2 (
    pMachineTypeRow $ MachineTypeRow {
      _machineTypePK = MT.pMachineTypeId . MT.MachineTypeId $ AGG.groupBy ,
      _machineType = MT.pMachineType $ MT.MachineType {
        MT.kind = AGG.min ,
        MT.machineTypeName = AGG.min ,
        MT.machineTypeManufacturer = AGG.min }} ,
    MT.pMachineTypeId . MT.MachineTypeId $ AGG.count )
  stripMachineType ::
    Query (MachineTypesTable, MT.MachineTypeId' DBInt8) ->
    Query (MachineTypesTable, DBInt8)
  stripMachineType q = proc () -> do
    (columns, mt) <- q -< ()
    returnA -< (columns, MT.getMachineTypeId mt)
  in stripMachineType . orderBy (asc(MT.machineTypeManufacturer . _machineType . fst) <>
    asc(MT.machineTypeName . _machineType . fst)) $ mkAggregatedQuery query'

singleMachineTypeQuery :: Either String Int -> Query MachineTypesTable
singleMachineTypeQuery machineTypeSid = proc () -> do
  machineTypeRow <- queryTable MTD.machineTypesTable -< ()
  restrict -< case machineTypeSid of
    Right(machineTypeId) ->
      MTD._machineTypePK machineTypeRow .=== (MT.MachineTypeId . pgInt4 $ machineTypeId)
    Left(machineTypeName) ->
      ((MT.machineTypeName . MTD._machineType $ machineTypeRow) .== pgString machineTypeName)
  returnA -< machineTypeRow

machinesInUpkeepQuery''' :: U.UpkeepId -> Query (MachinesTable, MachineTypesTable, UpkeepMachinesTable)
machinesInUpkeepQuery''' upkeepId = proc () -> do
  upkeepMachineRow <- machinesInUpkeepQuery upkeepId -< ()
  machineRow <- joinL machinesTable machinePK -< UMD._machineFK upkeepMachineRow
  machineTypeRow <- joinL MTD.machineTypesTable MTD.machineTypePK -< _machineTypeFK machineRow
  returnA -< (machineRow, machineTypeRow, upkeepMachineRow)

machinesInUpkeepQuery'' ::
  U.UpkeepId ->
  Query (MachinesTable, MachineTypesTable, ContactPersonsLeftJoinTable, UpkeepMachinesTable)
machinesInUpkeepQuery'' upkeepId = let
  joined = leftJoin
    (machinesInUpkeepQuery''' upkeepId)
    contactPersonsQuery
    (\((machineRow,_,_), contactPersonRow) ->
      view contactPersonFK machineRow .== (maybeToNullable . Just . sel1 $ contactPersonRow))
  in proc () -> do
    ((a,b,c), contactPersons) <- joined -< ()
    returnA -< (a,b,contactPersons,c)

machinesQ :: C.CompanyId -> Query (MachinesTable, MachineTypesTable)
machinesQ companyId = orderBy (asc(\(machine', _) -> C.getCompanyId . _companyFK $ machine')) $ proc () -> do
  machineRow <- joinL machinesTable companyFK -< fmap pgInt4 companyId
  mt <- joinL MTD.machineTypesTable MTD.machineTypePK -< _machineTypeFK machineRow
  returnA -< (machineRow, mt)

machinesInCompanyQuery ::
  C.CompanyId ->
  Query (MachinesTable, MachineTypesTable, ContactPersonsLeftJoinTable, UpkeepsLeftJoinTable)
machinesInCompanyQuery companyId = let
  machinesContactPersonsQ = leftJoin
    (machinesQ companyId)
    contactPersonsQuery
    (\((machineRow,_), cpRow) -> view contactPersonFK machineRow .== (maybeToNullable . Just . sel1 $ cpRow))
  lastUpkeepForMachineQ = AGG.aggregate (p2(AGG.max, M.pMachineId (M.MachineId AGG.groupBy))) $ proc () -> do
    umRow <- queryTable upkeepMachinesTable -< ()
    upkeepRow <- joinQ superUpkeepQuery upkeepPK -< UMD._upkeepFK umRow
    returnA -< (U.upkeepDate . _upkeep $ upkeepRow, UMD._machineFK umRow)
  lastUpkeepsQ = proc () -> do
    (upkeepDate, upkeepMachineFK) <- lastUpkeepForMachineQ -< ()
    umRow <- joinL upkeepMachinesTable machineFK -< upkeepMachineFK
    upkeepRow <- joinQ superUpkeepQuery upkeepPK -< UMD._upkeepFK umRow
    restrict -< (U.upkeepDate . _upkeep $ upkeepRow) .== upkeepDate
    returnA -< (upkeepRow, upkeepMachineFK)
  withLastUpkeep = leftJoin
    machinesContactPersonsQ
    lastUpkeepsQ
    (\(machine', upkeep') -> (_machinePK . fst . fst $ machine') .=== snd upkeep')
  in proc () -> do
    (((a,b),c),(d,_ :: M.MachineId' MBInt)) <- withLastUpkeep -< ()
    returnA -< (a,b,c,d)

machinesInCompanyQuery' :: U.UpkeepId -> Query (MachinesTable, MachineTypesTable)
machinesInCompanyQuery' upkeepId = let
  companyPKQ = distinct $ proc () -> do
    upkeepMachineRow <- joinL upkeepMachinesTable UMD.upkeepFK -< fmap pgInt4 upkeepId
    machineRow <- joinL machinesTable machinePK -< UMD._machineFK upkeepMachineRow
    companyRow <- joinL companiesTable companyPK -< _companyFK machineRow
    returnA -< _companyPK companyRow
  in proc () -> do
    companyId <- companyPKQ -< ()
    machineRow <- joinL machinesTable companyFK -< companyId
    machineTypeRow <- joinL MTD.machineTypesTable machineTypePK -< _machineTypeFK machineRow
    returnA -< (machineRow, machineTypeRow)

companyUpkeepsQuery :: C.CompanyId -> Query UpkeepsTable
companyUpkeepsQuery companyId = let
  upkeepsQuery' = proc () -> do
    machineRow <- joinL machinesTable companyFK -< fmap pgInt4 companyId
    upkeepMachinesRow <- joinL upkeepMachinesTable UMD.machineFK -< _machinePK machineRow
    upkeepRow <- joinQ superUpkeepQuery upkeepPK -< UMD._upkeepFK upkeepMachinesRow
    restrict -< U.upkeepClosed . _upkeep $ upkeepRow
    returnA -< upkeepRow
  aggregatedUpkeepsQuery = AGG.aggregate (pUpkeepRow $ UpkeepRow (U.pUpkeepId . U.UpkeepId $ AGG.groupBy)
    (U.pUpkeep $ U.Upkeep AGG.min AGG.boolOr AGG.min AGG.min AGG.min AGG.boolOr)
    (U.pUpkeepId . U.UpkeepId $ AGG.min)) upkeepsQuery'
  orderedUpkeepQuery = orderBy (asc(\u -> U.upkeepDate . _upkeep $ u)) $ aggregatedUpkeepsQuery
  in orderedUpkeepQuery

-- | query, that returns expanded machine type, not just the id
expandedMachinesQuery :: Maybe Int -> Query (MachinesTable, MachineTypesTable)
expandedMachinesQuery machineId = proc () -> do
  machineRow <- queryTable machinesTable -< ()
  machineTypesRow <- joinL MTD.machineTypesTable MTD.machineTypePK -< _machineTypeFK machineRow
  restrict -< (case machineId of
    Just(machineId'') -> ((M.MachineId . pgInt4 $ machineId'') .=== _machinePK machineRow)
    Nothing -> pgBool True)
  returnA -< (machineRow, machineTypesRow)

machineDetailQuery ::
  M.MachineId ->
  Query (MachinesTable, MachineTypesTable, ContactPersonsLeftJoinTable)
machineDetailQuery (M.MachineId machineId) = let
  joined = leftJoin
    (expandedMachinesQuery $ Just machineId)
    contactPersonsQuery
    (\((machineRow,_), cpRow) -> _contactPersonFK machineRow .== (maybeToNullable . Just . sel1 $ cpRow))
  in proc () -> do
    ((m,mt), cp) <- joined -< ()
    returnA -< (m, mt, cp)

machinesInCompanyByUpkeepQuery :: U.UpkeepId -> Query (CompanyPK, MachinesTable, MachineTypesTable)
machinesInCompanyByUpkeepQuery upkeepId = let
  companyPKQuery = limit 1 $ proc () -> do
    upkeepMachineRow <- joinL upkeepMachinesTable UMD.upkeepFK -< fmap pgInt4 upkeepId
    machineRow <- joinL machinesTable machinePK -< UMD._machineFK upkeepMachineRow
    returnA -< _companyFK machineRow
  in proc () -> do
    companyPK' <- companyPKQuery -< ()
    machineRow <- joinL machinesTable companyFK -< companyPK'
    mt <- joinL MTD.machineTypesTable MTD.machineTypePK -< _machineTypeFK machineRow
    returnA -< (companyPK', machineRow, mt)

expandedUpkeepsQuery2 :: U.UpkeepId -> Query (UpkeepsTable, UpkeepMachinesTable)
expandedUpkeepsQuery2 upkeepId = let
  upkeepKey = fmap pgInt4 upkeepId
  in proc () -> do
    upkeepRow <- joinQ (upkeepQuery Nothing) upkeepPK -< upkeepKey
    upkeepMachineRow <- joinL upkeepMachinesTable UMD.upkeepFK -< upkeepKey
    returnA -< (upkeepRow, upkeepMachineRow)

expandedUpkeepsQuery :: Query (UpkeepsTable, UpkeepMachinesTable)
expandedUpkeepsQuery = proc () -> do
  upkeepRow <- superUpkeepQuery -< ()
  upkeepMachineRow <- joinL upkeepMachinesTable UMD.upkeepFK -< _upkeepPK upkeepRow
  returnA -< (upkeepRow, upkeepMachineRow)

contactPersonsByIdQuery :: C.CompanyId -> Query ContactPersonsTable
contactPersonsByIdQuery companyId = proc () -> do
  contactPersonRow <- contactPersonsQuery -< ()
  restrict -< (C.CompanyId . sel2 $ contactPersonRow) .=== fmap pgInt4 companyId
  returnA -< contactPersonRow

nextServiceMachinesQuery :: C.CompanyId -> Query MachinesTable
nextServiceMachinesQuery (C.CompanyId companyId) = proc () -> do
  machineRow <- queryTable machinesTable -< ()
  restrict -< _companyFK machineRow .=== (C.CompanyId . pgInt4 $ companyId)
  returnA -< machineRow

joinL ::
  (Default EqPP aKey aKey,
    Default ColumnMaker a a)
  =>
  (Table b a) ->
  Getting aKey a aKey ->
  QueryArr aKey a
joinL table getKey = joinQ (queryTable table) getKey

joinQ :: (Default EqPP aKey aKey, Default ColumnMaker a a) =>
  Query a ->
  Getting aKey a aKey ->
  QueryArr aKey a
joinQ query getKey = proc aKey -> do
  aRows <- query -< ()
  restrict -< view getKey aRows .=== aKey
  returnA -< aRows

nextServiceUpkeepsQuery :: M.MachineId -> Query (UpkeepsTable, UpkeepMachinesTable)
nextServiceUpkeepsQuery machineId = proc () -> do
  upkeepMachineRow <- joinL upkeepMachinesTable UMD.machineFK -< fmap pgInt4 machineId
  upkeepRow <- joinQ superUpkeepQuery upkeepPK -< UMD._upkeepFK upkeepMachineRow
  returnA -< (upkeepRow, upkeepMachineRow)

upkeepsDataForMachine :: M.MachineId -> Query (UpkeepsTable, UpkeepMachinesTable, EmployeeLeftJoinTable)
upkeepsDataForMachine machineId = let
  upkeepEQ :: Query (UpkeepEmployeesTable, EmployeeTable)
  upkeepEQ = proc () -> do
    upkeepEmployeeRow <- queryTable upkeepEmployeesTable -< ()
    employeeRow <- join . queryTable $ employeesTable -< sel2 upkeepEmployeeRow
    returnA -< (upkeepEmployeeRow, employeeRow)
  upkeepQ :: Query (UpkeepsTable, UpkeepMachinesTable)
  upkeepQ = proc () -> do
    machineRow <- joinL machinesTable machinePK -< fmap pgInt4 machineId
    upkeepMachineRow <- joinL upkeepMachinesTable UMD.machineFK -< _machinePK machineRow
    upkeepRow <-  joinQ (upkeepQuery Nothing) upkeepPK -< UMD._upkeepFK upkeepMachineRow
    returnA -< (upkeepRow, upkeepMachineRow)
  ljQ :: Query ((UpkeepsTable, UpkeepMachinesTable), (
    (Column (Nullable PGInt4), Column (Nullable PGInt4), Column (Nullable PGInt4)), EmployeeLeftJoinTable))
  ljQ = leftJoin upkeepQ upkeepEQ (\((u, _), (ue, _)) -> _upkeepPK u .=== (U.UpkeepId . sel1 $ ue))
  in orderBy (desc (U.upkeepDate . _upkeep . sel1)) $ proc () -> do
    ((u, um), (_, e)) <- ljQ -< ()
    returnA -< (u, um, e)

nextServiceUpkeepSequencesQuery :: M.MachineId -> Query UpkeepSequencesTable
nextServiceUpkeepSequencesQuery (M.MachineId machineId) = proc () -> do
  machineRow <- joinL machinesTable machinePK -< M.MachineId . pgInt4 $ machineId
  machineTypeRow <- joinL MTD.machineTypesTable MTD.machineTypePK -< _machineTypeFK machineRow
  upkeepSequenceRow <- upkeepSequencesByIdQuery' -< MTD._machineTypePK machineTypeRow
  returnA -< upkeepSequenceRow

expandedUpkeepsByCompanyQuery :: C.CompanyId -> Query
  (UpkeepsTable, UpkeepMachinesTable, MachinesTable, MachineTypesTable)
expandedUpkeepsByCompanyQuery companyId = let
  upkeepsWithMachines = proc () -> do
    upkeepRow <- superUpkeepQuery -< ()
    upkeepMachineRow <- joinL upkeepMachinesTable UMD.upkeepFK -< _upkeepPK upkeepRow
    machineRow <- joinL machinesTable machinePK -< UMD._machineFK upkeepMachineRow
    machineTypeRow <- joinL MTD.machineTypesTable MTD.machineTypePK -< _machineTypeFK machineRow
    restrict -< _companyFK machineRow .=== fmap pgInt4 companyId
    returnA -< (upkeepRow, upkeepMachineRow, machineRow, machineTypeRow)
  in orderBy (
    desc (\(u,_,_,_) -> U.upkeepDate . _upkeep $ u) <>
    desc (\(u,_,_,_) -> U.getUpkeepId $ _upkeepPK $ u))
      upkeepsWithMachines

singleEmployeeQuery :: Int -> Query (EmployeeTable)
singleEmployeeQuery employeeId = proc () -> do
  employeeRow <- join employeesQuery -< (pgInt4 employeeId)
  returnA -< employeeRow

employeesInUpkeeps :: [U.UpkeepId] -> Query (UpkeepPK, EmployeeTable)
employeesInUpkeeps upkeepIds = proc () -> do
  upkeepRow <- superUpkeepQuery -< ()
  restrict -< in_ ((pgInt4 . U.getUpkeepId) `fmap` upkeepIds) (U.getUpkeepId . _upkeepPK $ upkeepRow)
  upkeepEmployeeRow <- join . queryTable $ upkeepEmployeesTable -< U.getUpkeepId . _upkeepPK $ upkeepRow
  employeeRow <- join . queryTable $ employeesTable -< sel2 upkeepEmployeeRow
  returnA -< (_upkeepPK upkeepRow, employeeRow)

data PlannedUpkeepType = ServiceUpkeep | CalledUpkeep
data DealWithSubtasks = NormalTasks | Subtasks deriving Eq

groupedPlannedUpkeepsQuery :: PlannedUpkeepType -> Query (UpkeepsTable, DBInt, DBInt, (C.CompanyId' DBInt, CompanyCore))
groupedPlannedUpkeepsQuery = groupedPlannedUpkeepsQuery' NormalTasks

groupedPlannedUpkeepsQuery' :: DealWithSubtasks -> PlannedUpkeepType -> Query (UpkeepsTable, DBInt, DBInt, (C.CompanyId' DBInt, CompanyCore))
groupedPlannedUpkeepsQuery' dealWithSubtasks plannedUpkeepType = let
  modifier = case plannedUpkeepType of
    ServiceUpkeep -> not
    CalledUpkeep  -> id
  plannedUpkeepsQuery = proc () -> do
    upkeepRow <- upkeepQuery (Just dealWithSubtasks) -< ()
    restrict -< not . U.upkeepClosed . _upkeep $ upkeepRow
    restrict -< modifier . U.setDate . _upkeep $ upkeepRow
    upkeepMachinesRow <- joinL upkeepMachinesTable UMD.upkeepFK -< _upkeepPK upkeepRow
    machineRow <- joinL machinesTable machinePK -< UMD._machineFK upkeepMachinesRow
    machineTypeRow <- joinL MTD.machineTypesTable MTD.machineTypePK -< _machineTypeFK machineRow
    companyRow <- joinL companiesTable companyPK -< _companyFK machineRow
    returnA -< (upkeepRow, MT.kind . MTD._machineType $ machineTypeRow, M.upkeepBy . _machine $ machineRow ,
      (_companyPK companyRow, _companyCore companyRow))
  in orderBy (asc(view (_1 . upkeep . U.upkeepDateL))) $
    AGG.aggregate (p4 (pUpkeepRow $ UpkeepRow (U.pUpkeepId . U.UpkeepId $ AGG.groupBy)
    (U.pUpkeep $ U.Upkeep AGG.min AGG.boolOr AGG.min AGG.min AGG.min AGG.boolOr) (U.pUpkeepId . U.UpkeepId $ AGG.min), AGG.min, AGG.max,
      p2(C.pCompanyId . C.CompanyId $ AGG.min, C.pCompany $ C.Company AGG.min AGG.min AGG.min AGG.boolOr)))
    plannedUpkeepsQuery

singleContactPersonQuery :: Int -> Query (ContactPersonsTable, CompanyRead)
singleContactPersonQuery contactPersonId = proc () -> do
  contactPersonRow <- join contactPersonsQuery -< pgInt4 contactPersonId
  companyRow <- queryTable companiesTable -< ()
  restrict -< (C.getCompanyId . _companyPK) companyRow .== sel2 contactPersonRow
  returnA -< (contactPersonRow, companyRow)

machinesInUpkeepQuery :: U.UpkeepId -> Query UpkeepMachinesTable
machinesInUpkeepQuery upkeepId =
  joinL upkeepMachinesTable UMD.upkeepFK <<< arr (const $ fmap pgInt4 upkeepId)

extraFieldsPerKindQuery :: Int -> Query ExtraFieldSettingsTable
extraFieldsPerKindQuery machineKind = orderBy (asc $ sel3) $ proc () -> do
  extraFieldRow <- extraFieldSettingsQuery -< ()
  restrict -< pgInt4 machineKind .== sel2 extraFieldRow
  returnA -< extraFieldRow

machineIdsHavingKind :: Int -> Query MachinePK
machineIdsHavingKind machineTypeKind = proc () -> do
  machineTypeRow <- queryTable MTD.machineTypesTable -< ()
  restrict -< pgInt4 machineTypeKind .== (MT.kind . MTD._machineType $ machineTypeRow)
  machineRow <- joinL machinesTable machineTypeFK -< MTD._machineTypePK machineTypeRow
  returnA -< _machinePK machineRow

employeeIdsInUpkeep :: U.UpkeepId -> Query DBInt
employeeIdsInUpkeep upkeepId = proc () -> do
  (_, employeeId, _) <- join . queryTable $ upkeepEmployeesTable -< pgInt4 . U.getUpkeepId $ upkeepId
  returnA -< employeeId

employeesInUpkeep :: U.UpkeepId -> Query EmployeeTable
employeesInUpkeep upkeepId = proc () -> do
  employeeId <- employeeIdsInUpkeep upkeepId -< ()
  employeeRow <- join employeesQuery -< employeeId
  returnA -< employeeRow

extraFieldsForMachineQuery :: M.MachineId -> Query (ExtraFieldsTable, ExtraFieldSettingsTable)
extraFieldsForMachineQuery (M.MachineId machineId) = orderBy (asc $ sel3 . snd) $ proc () -> do
  machineRow <- joinL machinesTable machinePK -< (M.MachineId . pgInt4 $ machineId)
  extraFieldRow <- extraFieldsQuery -< ()
  restrict -< (M.MachineId . sel2 $ extraFieldRow) .=== _machinePK machineRow
  extraFieldSettingRow <- join extraFieldSettingsQuery -< sel1 extraFieldRow
  returnA -< (extraFieldRow, extraFieldSettingRow)

mainEmployeesInDayQ ::
  Day ->
  Query EmployeeTable
mainEmployeesInDayQ day = distinct $ proc () -> do
  upkeepRow <- superUpkeepQuery -< ()
  restrict -< (U.upkeepDate . _upkeep $ upkeepRow) .== pgDay day
  upkeepEmployeeRow <- join . queryTable $ upkeepEmployeesTable -< U.getUpkeepId . _upkeepPK $ upkeepRow
  restrict -< pgInt4 0 .== sel3 upkeepEmployeeRow
  employeeRow <- join employeesQuery -< sel2 upkeepEmployeeRow
  returnA -< employeeRow

dailyPlanQuery :: Maybe E.EmployeeId -> Day -> Query (UpkeepsTable, Column (PGArray PGInt4))
dailyPlanQuery employeeId' day = let
  q = proc () -> do
    upkeepRow <- superUpkeepQuery -< ()
    restrict -< (U.upkeepDate . _upkeep $ upkeepRow) .== pgDay day
    upkeepEmployeeRow <- join . queryTable $ upkeepEmployeesTable -< U.getUpkeepId . _upkeepPK $ upkeepRow
    restrict -< case employeeId' of
      Just (E.EmployeeId employeeId) -> pgInt4 employeeId .== sel2 upkeepEmployeeRow
      Nothing -> pgBool False -- todo return no results earlier than here in db query
    restrict -< pgInt4 0 .== sel3 upkeepEmployeeRow
    upkeepEmployeeRowData <- join . queryTable $ upkeepEmployeesTable -< U.getUpkeepId . _upkeepPK $ upkeepRow
    returnA -< (upkeepRow, sel2 upkeepEmployeeRowData)
  in AGG.aggregate (p2(pUpkeepRow (UpkeepRow (U.pUpkeepId . U.UpkeepId $ AGG.groupBy)
    (U.pUpkeep $ U.Upkeep AGG.min AGG.boolOr AGG.min AGG.min AGG.min AGG.boolOr) (U.pUpkeepId . U.UpkeepId $ AGG.min) ), aggrArray)) q

tasksForEmployeeQuery :: E.EmployeeId -> Query TasksTable
tasksForEmployeeQuery (E.EmployeeId employeeId) = proc () -> do
  taskRow <- queryTable tasksTable -< ()
  taskEmployeeRow <- join . queryTable $ taskEmployeesTable -< sel1 taskRow
  restrict -< pgInt4 employeeId .== sel2 taskEmployeeRow
  returnA -< taskRow

getTaskQuery :: T.TaskId -> Query TasksTable
getTaskQuery (T.TaskId taskIdInt) = proc () -> do
  taskRow <- join . queryTable $ tasksTable -< pgInt4 taskIdInt
  returnA -< taskRow

lastRecommendationQuery :: C.CompanyId -> Query UpkeepsTable
lastRecommendationQuery companyId =
  limit 1 . orderBy (desc (U.upkeepDate . _upkeep)) $ proc () -> do
    machineRow <- joinL machinesTable companyFK -< fmap pgInt4 companyId
    upkeepMachineRow <- joinL upkeepMachinesTable UMD.machineFK -< _machinePK machineRow
    upkeepRow <- joinQ superUpkeepQuery upkeepPK -< UMD._upkeepFK upkeepMachineRow
    restrict -< U.upkeepClosed . _upkeep $ upkeepRow
    returnA -< upkeepRow

takenColoursQuery :: Query DBText
takenColoursQuery = distinct $ proc () -> do
  employeeRow <- queryTable employeesTable -< ()
  returnA -< sel5 employeeRow

aggrArray :: AGG.Aggregator (Column a) (Column (PGArray a))
aggrArray = IAGG.makeAggr . HPQ.AggrOther $ "array_agg"

notesForUpkeep ::
  U.UpkeepId ->
  Query (MachinePK, DBText, DBText, DBInt)
notesForUpkeep upkeepId = proc () -> do
  upkeepMachineRow <- joinL upkeepMachinesTable UMD.upkeepFK -< fmap pgInt4 upkeepId
  machinesRow <- joinL machinesTable machinePK -< UMD._machineFK upkeepMachineRow
  machineTypesRow <- joinL MTD.machineTypesTable MTD.machineTypePK -< _machineTypeFK machinesRow
  returnA -< (
    UMD._machineFK upkeepMachineRow ,
    MT.machineTypeName . MTD._machineType $ machineTypesRow ,
    UM.upkeepMachineNote . UMD._upkeepMachine $ upkeepMachineRow ,
    MT.kind . MTD._machineType $ machineTypesRow )

multiEmployeeQuery :: [Int] -> Query EmployeeTable
multiEmployeeQuery employeeIds = proc () -> do
  employeeRow <- employeesQuery -< ()
  restrict -< in_ (pgInt4 `fmap` employeeIds) (sel1 employeeRow)
  returnA -< employeeRow

photosInUpkeepQuery :: U.UpkeepId -> Query DBInt
photosInUpkeepQuery (U.UpkeepId upkeepId) = proc () -> do
  upkeepPhotosRow <- queryTable upkeepPhotosTable -< ()
  restrict -< sel2 upkeepPhotosRow .== pgInt4 upkeepId
  returnA -< sel1 upkeepPhotosRow

upkeepForPhotoQ :: P.PhotoId -> Query DBInt
upkeepForPhotoQ (P.PhotoId pInt) = proc () -> do
  upkeepPhotosRow <- queryTable upkeepPhotosTable -< ()
  restrict -< sel1 upkeepPhotosRow .== pgInt4 pInt
  returnA -< sel2 upkeepPhotosRow

companyInUpkeepQuery :: U.UpkeepId -> Query CompanyCore
companyInUpkeepQuery upkeepId = distinct $ proc () -> do
  upkeepMachineRow <- joinL upkeepMachinesTable UMD.upkeepFK -< fmap pgInt4 upkeepId
  machineRow <- joinL machinesTable machinePK -< UMD._machineFK upkeepMachineRow
  companyRow <- joinL companiesTable companyPK -< _companyFK machineRow
  returnA -< _companyCore companyRow

machinesForTypeQ ::
  MachineTypeSid ->
  Query (MachinesTable, CompanyRead)
machinesForTypeQ (MachineTypeById machineTypeId) =
  orderBy (asc (C.companyName . _companyCore . snd) <> asc (view (_1 . machine . M.serialNumberL))) $
    proc () -> do
      machineRow <- joinL machinesTable machineTypeFK -< fmap pgInt4 machineTypeId
      companyRow <- joinL companiesTable companyPK -< _companyFK machineRow
      returnA -< (machineRow, companyRow)
machinesForTypeQ (MachineTypeByName _) = undefined

runMachinesInCompanyQuery ::
  C.CompanyId ->
  Connection ->
  IO [(M.MachineId, M.Machine, C.CompanyId, MT.MachineTypeId,
    MT.MachineType, Maybe CP.ContactPerson, M.MachineId' (Maybe Int), Maybe U.Upkeep)]
runMachinesInCompanyQuery companyId connection = do
  rows <- runQuery connection (machinesInCompanyQuery companyId)
  let
    mapRow (row @ (_,_,_,_)) = let
      (machineType' :: MachineTypeRecord, contactPerson :: MaybeContactPersonMapped) =
        (sel2 row, convert . sel3 $ row)
      (machineRecord :: MachineRecord) = sel1 row
      in (_machinePK machineRecord, _machine machineRecord, _companyFK machineRecord, _machineTypePK machineType',
        _machineType machineType', sel3 contactPerson, view linkageFK machineRecord, fmap _upkeep . mapMaybeUpkeep . sel4 $ row)
  return . nubBy (\a0 a1 -> sel1 a0 == sel1 a1) . fmap mapRow $ rows

runExpandedMachinesQuery' ::
  Maybe Int ->
  Connection ->
  IO [(MachineRecord, MachineTypeRecord)]
runExpandedMachinesQuery' machineId connection =
  runQuery connection $ expandedMachinesQuery machineId

runCompanyUpkeepsQuery ::
  C.CompanyId ->
  Connection ->
  IO [UpkeepRow]
runCompanyUpkeepsQuery companyId connection =
  fmap
    (over (mapped . upkeepSuper) (\y -> fmap (U.UpkeepId) (U.getUpkeepId y) ) )
    (runQuery connection (companyUpkeepsQuery companyId))

convertExpanded ::
  (MachineRecord, MachineTypeRecord) ->
  (M.MachineId, M.Machine, C.CompanyId, MT.MachineTypeId, MT.MachineType)
convertExpanded (m, mt) = let
  in (_machinePK m, _machine m, _companyFK m, _machineTypePK mt, _machineType mt)

runExpandedMachinesQuery ::
  Maybe Int ->
  Connection ->
  IO[(M.MachineId, M.Machine, C.CompanyId, MT.MachineTypeId, MT.MachineType)]
runExpandedMachinesQuery machineId connection = do
  rows <- runExpandedMachinesQuery' machineId connection
  return $ fmap convertExpanded rows

runMachineTypesQuery' :: String -> Connection -> IO[Text]
runMachineTypesQuery' mid connection = runQuery connection (machineTypesQuery' mid)

runMachinesInCompanyByUpkeepQuery ::
  U.UpkeepId ->
  Connection ->
  IO[(C.CompanyId, (M.MachineId, M.Machine, C.CompanyId, MT.MachineTypeId, MT.MachineType))]
runMachinesInCompanyByUpkeepQuery upkeepId connection = do
  rows <- runQuery connection (machinesInCompanyByUpkeepQuery upkeepId)
  let _ = rows :: [(C.CompanyId, MachineRecord, MachineTypeRecord)]
  return $ map (\(cId,a,b) -> (cId, convertExpanded (a,b))) rows

initiateConnection :: IO Connection
initiateConnection = let
  connectInfo = defaultConnectInfo {
    connectUser = "haskell" ,
    connectDatabase = "crm" ,
    connectPassword = "haskell" ,
    connectHost = "localhost" }
  in connect connectInfo

withConnection :: (MonadIO m) => (Connection -> m a) -> m a
withConnection runQ = do
  conn <- liftIO initiateConnection
  result <- runQ conn
  liftIO $ close conn
  return result

insertExtraFields :: M.MachineId -> [(EF.ExtraFieldId, Text)] -> Connection -> IO ()
insertExtraFields machineId extraFields connection =
  forM_ extraFields $ \(extraFieldId, extraFieldValue) ->
    runInsert connection extraFieldsTable
      (pgInt4 $ EF.getExtraFieldId extraFieldId, pgInt4 $ M.getMachineId machineId, pgStrictText extraFieldValue) >> return ()

getPhoto ::
  Connection ->
  Int ->
  IO ByteString
getPhoto connection photoId = do
  let q = " select data from photos where id = ? "
  result <- query connection q (Only photoId)
  return $ fromOnly $ head $ result

addPhoto ::
  Connection ->
  ByteString ->
  IO [Int]
addPhoto connection photo = do
  let q = " insert into photos(data) values (?) returning id "
  newIds <- query connection q (Only $ Binary photo)
  let ints = map (\(Only id') -> id') newIds
  return ints

deletePhoto ::
  Connection ->
  Int ->
  IO ()
deletePhoto connection photoId = do
  let q = " delete from photos where id = ? "
  _ <- execute connection q (Only photoId)
  return ()

updatePhoto ::
  Connection ->
  Int ->
  ByteString ->
  IO ()
updatePhoto connection photoId photo = do
  let q = " update photos set data = ? where id = ? "
  _ <- execute connection q (Binary photo, photoId)
  return ()

singleRowOrColumn ::
  Monad m =>
  [a] ->
  ExceptT (Reason r) m a
singleRowOrColumn result = case result of
  row : xs | null xs -> return row
  [] -> throwError $ InputError $ ParseError "no record"
  _ -> throwError $ InputError $ ParseError "more than one record failure"
