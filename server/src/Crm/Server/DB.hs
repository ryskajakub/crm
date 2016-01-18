{-# OPTIONS -fno-warn-missing-signatures #-}   

{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Crm.Server.DB (
  -- tables
  companiesTable ,
  machinesTable ,
  machineTypesTable ,
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
  upkeepEmployeesTable ,
  taskEmployeesTable ,
  tasksTable ,
  upkeepPhotosTable ,
  -- basic queries
  extraFieldSettingsQuery ,
  extraFieldsQuery ,
  machineTypesQuery ,
  upkeepsQuery ,
  upkeepMachinesQuery ,
  employeesQuery ,
  upkeepSequencesQuery ,
  machinePhotosQuery ,
  getPhoto ,
  singleEmployeeQuery ,
  contactPersonsQuery ,
  -- manipulations
  addPhoto ,
  deletePhoto ,
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
  -- manipulations
  insertExtraFields ,
  -- helpers
  withConnection ,
  singleRowOrColumn ,
  mapUpkeeps ,
  initiateConnection ,
  mapMachineDate ,
  -- tables
  MachineRow'' (..) ,
  -- mappings
  MachineRecord ,
  ColumnToRecordDeep ,
  convert ,
  convertDeep ,
  MaybeContactPersonMapped ,
  MachineTypeMapped ,
  ContactPersonMapped ,
  CompanyMapped ,
  UpkeepMapped ,
  MaybeEmployeeMapped ,
  EmployeeMapped ,
  TaskMapped ,
  UpkeepSequenceMapped ,
  UpkeepMachineMapped ,
  PhotoMetaMapped ,
  ExtraFieldSettingsMapped ,
  ExtraFieldMapped ,
  MachineMapped ,
  -- types
  MachineRecord' ) where

import           Control.Arrow                        (returnA, (^<<))
import           Control.Applicative                  ((<*>), pure)
import           Control.Monad                        (forM_)
import           Control.Lens                         (view, _2, over, makeLenses, _1, mapped)
import           Data.List                            (intersperse)
import           Data.Monoid                          ((<>))

import           Control.Monad.Error.Class            (throwError)
import           Control.Monad.Trans.Except           (ExceptT)
import           Control.Monad.IO.Class               (MonadIO, liftIO)
import           Data.Profunctor.Product              (p1, p2, p3, p4, p5, p6, p7, p13)
import           Data.Time.Calendar                   (Day)
import           Data.Tuple.All                       (Sel1, sel1, sel2, sel3, sel4, uncurryN, upd2, sel6)
import           Data.ByteString.Lazy                 (ByteString)
import           Data.Text                            (Text, pack)
import           Database.PostgreSQL.Simple           (ConnectInfo(..), Connection, defaultConnectInfo, 
                                                      connect, close, query, Only(..), Binary(..), execute)
import           Opaleye.QueryArr                     (Query, QueryArr)
import           Opaleye.Table                        (Table(Table), required, queryTable, optional)
import           Opaleye.Column                       (Column, Nullable, isNull)
import           Opaleye.Order                        (orderBy, asc, limit, PGOrd, desc)
import           Opaleye.RunQuery                     (runQuery)
import           Opaleye.Operators                    (restrict, lower, (.==), (.||), in_, (./=), (.===))
import           Opaleye.PGTypes                      (pgInt4, PGDate, PGBool, PGInt4, PGInt8, 
                                                      PGText, pgStrictText, pgBool, PGFloat8, 
                                                      pgString, PGBytea, pgDay, PGArray)
import qualified Opaleye.Aggregate                    as AGG
import           Opaleye.Join                         (leftJoin)
import           Opaleye.Distinct                     (distinct)
import           Opaleye                              (runInsert)
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ
import qualified Opaleye.Internal.Column              as C
import qualified Opaleye.Internal.Aggregate           as IAGG
import           Data.Profunctor.Product.TH

import           Rest.Types.Error                     (DataError(ParseError), Reason(InputError))
import           TupleTH

import qualified Crm.Shared.Company                   as C
import qualified Crm.Shared.Employee                  as E
import qualified Crm.Shared.Task                      as T
import qualified Crm.Shared.ContactPerson             as CP
import qualified Crm.Shared.Machine                   as M
import qualified Crm.Shared.MachineType               as MT
import qualified Crm.Shared.MachineKind               as MK
import qualified Crm.Shared.Upkeep                    as U
import qualified Crm.Shared.UpkeepSequence            as US
import qualified Crm.Shared.UpkeepMachine             as UM
import qualified Crm.Shared.PhotoMeta                 as PM
import qualified Crm.Shared.Photo                     as P
import qualified Crm.Shared.ExtraField                as EF

import           Crm.Server.Helpers                   (dayToYmd, maybeToNullable)


type DBInt = Column PGInt4
type DBInt8 = Column PGInt8
type DBText = Column PGText
type DBDate = Column PGDate
type DBBool = Column PGBool

type MBInt = Column (Nullable PGInt4)
type MBInt8 = Column (Nullable PGInt8)
type MBText = Column (Nullable PGText)
type MBDate = Column (Nullable PGDate)
type MBBool = Column (Nullable PGBool)

type ContactPersonsTable = (DBInt, DBInt, DBText, DBText, DBText)
type ContactPersonsLeftJoinTable = (Column (Nullable PGInt4), Column (Nullable PGInt4), 
  Column (Nullable PGText), Column (Nullable PGText), Column (Nullable PGText))
type ContactPersonsWriteTable = (Maybe DBInt, DBInt, DBText, DBText, DBText)

type UpkeepsTable = (DBInt, DBDate, DBBool, DBText, DBText, DBText)
type UpkeepsLeftJoinTable = (Column (Nullable PGInt4), Column (Nullable PGDate), Column (Nullable PGBool), 
  Column (Nullable PGText), Column (Nullable PGText), Column (Nullable PGText))
type UpkeepsWriteTable = (Maybe DBInt, DBDate, DBBool, DBText, DBText, DBText)

type UpkeepMachinesTable = (DBInt, DBText, DBInt, DBInt, DBBool, DBText, DBBool)
type UpkeepMachinesLeftJoinTable = (MBInt, MBText, MBInt, MBInt, MBBool, MBText, MBBool)

type EmployeeTable = (DBInt, DBText, DBText, DBText, DBText)
type EmployeeLeftJoinTable = (Column (Nullable PGInt4), Column (Nullable PGText), Column (Nullable PGText),
  Column (Nullable PGText), Column (Nullable PGText))
type EmployeeWriteTable = (Maybe DBInt, DBText, DBText, DBText, DBText)

type UpkeepSequencesTable = (DBInt, DBText, DBInt, DBInt, DBBool)

type PhotosMetaTable = (DBInt, DBText, DBText)

type MachinePhotosTable = (DBInt, DBInt)

type UpkeepPhotosTable = (DBInt, DBInt)

type ExtraFieldSettingsTable = (DBInt, DBInt, DBInt, DBText)
type ExtraFieldSettingsWriteTable = (Maybe DBInt, DBInt, DBInt, DBText)

type ExtraFieldsTable = (DBInt, DBInt, DBText)

type PasswordTable = (Column PGBytea)

type UpkeepEmployeesTable = (DBInt, DBInt, DBInt)

type TaskEmployeesTable = (DBInt, DBInt)
type TasksWriteTable = (Maybe DBInt, DBDate, DBText, Column (Nullable PGDate))
type TasksTable = (DBInt, DBDate, DBText, Column (Nullable PGDate))

type MachineTypesTable = (DBInt, DBInt, DBText, DBText)
type MachineTypesWriteTable = (Maybe DBInt, DBInt, DBText, DBText)

passwordTable :: Table PasswordTable PasswordTable
passwordTable = Table "password" $ p1 ( required "password" )

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

upkeepPhotosTable :: Table UpkeepPhotosTable UpkeepPhotosTable
upkeepPhotosTable = Table "upkeep_photos" $ p2 (
  required "photo_id" ,
  required "upkeep_id" )


type CompanyCore = C.Company' DBText DBText DBText
type CompanyCoords = C.Coordinates' (Column (Nullable PGFloat8)) (Column (Nullable PGFloat8))
type CompanyRead = C.CompanyTable' (C.CompanyId' DBInt) CompanyCore CompanyCoords
type CompanyWrite = C.CompanyTable' (C.CompanyId' (Maybe DBInt)) CompanyCore CompanyCoords

makeAdaptorAndInstance' ''C.Company'
makeAdaptorAndInstance' ''C.Coordinates'
makeAdaptorAndInstance' ''C.CompanyId'
makeAdaptorAndInstance' ''C.CompanyTable'

companiesTable :: Table CompanyWrite CompanyRead
companiesTable = Table "companies" $ pCompanyTable C.CompanyTable {
  C._companyPK = (pCompanyId C.CompanyId {
    C.getCompanyId = optional "id" }) ,
  C._companyCore = (pCompany C.Company {
    C.companyName = required "name" ,
    C.companyNote = required "note" ,
    C.companyAddress = required "address" }) ,
  C._companyCoords = (pCoordinates C.Coordinates {
    C.latitude = required "latitude" ,
    C.longitude = required "longitude" })}


data MachineRow'' machinePK companyId contactPersonId
    machineTypeId linkageId machine = MachineRow {
  _machinePK :: machinePK ,
  _companyFK :: companyId ,
  _contactPersonFK :: contactPersonId ,
  _machineTypeFK :: machineTypeId ,
  _linkageFK :: linkageId ,
  _machine :: machine }
makeLenses ''MachineRow''

type MachinePK = M.MachineId' DBInt
type MachineRow' machinePK = MachineRow'' 
  machinePK (C.CompanyId' DBInt) (Column (Nullable PGInt4)) DBInt (M.MachineId' (Column (Nullable PGInt4)))
  (M.Machine' (Column (Nullable PGDate)) DBInt DBInt DBText DBText DBText DBBool DBText)
type MachinesTable = MachineRow' MachinePK
type MachinesWrite = MachineRow' (M.MachineId' (Maybe DBInt))
type MachineRecord = MachineRow'' M.MachineId C.CompanyId (Maybe Int) Int (M.MachineId' (Maybe Int)) 
  M.Machine
type MachineRecord' = MachineRow'' M.MachineId C.CompanyId (Maybe Int) Int (M.MachineId' (Maybe Int))
  (M.Machine' (Maybe Day) Int Int Text Text Text Bool Text)

makeAdaptorAndInstance' ''M.Machine'
makeAdaptorAndInstance' ''M.MachineId'
makeAdaptorAndInstance' ''MachineRow''

machinesTable :: Table MachinesWrite MachinesTable
machinesTable = Table "machines" $ pMachineRow MachineRow {
  _machinePK = (pMachineId M.MachineId { 
    M.getMachineId = optional "id" }) ,
  _companyFK = (pCompanyId C.CompanyId {
    C.getCompanyId = required "company_id" }) ,
  _contactPersonFK = required "contact_person_id" ,
  _machineTypeFK = required "machine_type_id" ,
  _linkageFK = (pMachineId M.MachineId {
    M.getMachineId = required "linkage_id" }) ,
  _machine = (pMachine M.Machine {
    M.machineOperationStartDate = required "operation_start" ,
    M.initialMileage = required "initial_mileage" ,
    M.mileagePerYear = required "mileage_per_year" ,
    M.label_ = required "label" ,
    M.serialNumber = required "serial_number" ,
    M.yearOfManufacture = required "year_of_manufacture" ,
    M.archived = required "archived" ,
    M.note = required "note" })}


contactPersonsTable :: Table ContactPersonsWriteTable ContactPersonsTable
contactPersonsTable = Table "contact_persons" $ p5 (
  optional "id" ,
  required "company_id" ,
  required "name" ,
  required "phone" ,
  required "position" )

machineTypesTable :: Table MachineTypesWriteTable MachineTypesTable
machineTypesTable = Table "machine_types" $ p4 (
  optional "id" ,
  required "machine_kind" ,
  required "name" ,
  required "manufacturer" )

upkeepsTable :: Table UpkeepsWriteTable UpkeepsTable
upkeepsTable = Table "upkeeps" $ p6 (
  optional "id" ,
  required "date_" ,
  required "closed" ,
  required "work_hours" ,
  required "work_description" ,
  required "recommendation" )

upkeepMachinesTable :: Table UpkeepMachinesTable UpkeepMachinesTable
upkeepMachinesTable = Table "upkeep_machines" $ p7 (
  required "upkeep_id" ,
  required "note" ,
  required "machine_id" ,
  required "recorded_mileage" ,
  required "warranty" ,
  required "end_note" ,
  required "repair" )

employeesTable :: Table EmployeeWriteTable EmployeeTable
employeesTable = Table "employees" $ p5 (
  optional "id" ,
  required "name" ,
  required "contact" ,
  required "capabilities" ,
  required "colour" )

upkeepSequencesTable :: Table UpkeepSequencesTable UpkeepSequencesTable
upkeepSequencesTable = Table "upkeep_sequences" $ p5 (
  required "display_ordering" ,
  required "label" ,
  required "repetition" ,
  required "machine_type_id" ,
  required "one_time" )

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

machineTypesQuery :: Query MachineTypesTable
machineTypesQuery = queryTable machineTypesTable

upkeepsQuery :: Query UpkeepsTable
upkeepsQuery = queryTable upkeepsTable

upkeepMachinesQuery :: Query UpkeepMachinesTable
upkeepMachinesQuery = queryTable upkeepMachinesTable

employeesQuery :: Query EmployeeTable
employeesQuery = queryTable employeesTable

upkeepSequencesQuery :: Query UpkeepSequencesTable
upkeepSequencesQuery = queryTable upkeepSequencesTable

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
type UpkeepMapped = (U.UpkeepId, U.Upkeep)
type MaybeUpkeepMapped = (Maybe U.UpkeepId, Maybe U.Upkeep)
type EmployeeMapped = (E.EmployeeId, E.Employee)
type UpkeepSequenceMapped = (MT.MachineTypeId, US.UpkeepSequence)
type UpkeepMachineMapped = (U.UpkeepId, M.MachineId, UM.UpkeepMachine)
type PhotoMetaMapped = (P.PhotoId, PM.PhotoMeta)
type ExtraFieldSettingsMapped = (EF.ExtraFieldId, MK.MachineKindSpecific)
type ExtraFieldMapped = (EF.ExtraFieldId, M.MachineId, Text)
type TaskMapped = (T.TaskId, T.Task)

instance ColumnToRecord (Int, Text, Text, Text, Maybe Double, Maybe Double) CompanyMapped where
  convert tuple = let 
    company = (uncurryN $ const ((fmap . fmap . fmap) (const . const) C.Company)) tuple
    coordinates = pure C.Coordinates <*> $(proj 6 4) tuple <*> $(proj 6 5) tuple
    in (C.CompanyId $ $(proj 6 0) tuple, company, coordinates)
instance ColumnToRecord 
    (Int, Int, Maybe Int, Int, Maybe Int, Maybe Day, Int, Int, Text, Text, Text, Bool, Text)
    MachineMapped where
  convert tuple = let
    machineTuple = $(updateAtN 13 5) (fmap dayToYmd) tuple
    in (M.MachineId $ $(proj 13 0) tuple, C.CompanyId $ $(proj 13 1) tuple, CP.ContactPersonId `fmap` $(proj 13 2) tuple, 
      MT.MachineTypeId $ $(proj 13 3) tuple, M.MachineId `fmap` $(proj 13 4) tuple,
      (uncurryN $ const $ const $ const $ const $ const M.Machine) machineTuple)
instance ColumnToRecord (Int, Int, Text, Text) MachineTypeMapped where
  convert tuple = (MT.MachineTypeId $ $(proj 4 0) tuple, (uncurryN $ const MT.MachineType) 
    (upd2 (MK.dbReprToKind $ $(proj 4 1) tuple) tuple))
instance ColumnToRecord (Int, Int, Text, Text, Text) ContactPersonMapped where
  convert tuple = (CP.ContactPersonId $ $(proj 5 0) tuple, C.CompanyId $ $(proj 5 1) tuple, 
    (uncurryN $ const $ const CP.ContactPerson) tuple)
instance ColumnToRecord (Maybe Int, Maybe Int, Maybe Text, Maybe Text, Maybe Text) MaybeContactPersonMapped where
  convert tuple = let
    maybeCp = pure CP.ContactPerson <*> $(proj 5 2) tuple <*> $(proj 5 3) tuple <*> $(proj 5 4) tuple
    in (CP.ContactPersonId `fmap` $(proj 5 0) tuple, C.CompanyId `fmap` $(proj 5 1) tuple, maybeCp)
instance ColumnToRecord (Int, Day, Bool, Text, Text, Text) UpkeepMapped where
  convert tuple = let
    (_,a,b,c,d,e) = tuple
    in (U.UpkeepId $ $(proj 6 0) tuple, U.Upkeep (dayToYmd a) b c d e)
instance ColumnToRecord (Maybe Int, Maybe Day, Maybe Bool, Maybe Text, Maybe Text, Maybe Text) MaybeUpkeepMapped where
  convert tuple = let
    (_,a,b,c,d,e) = tuple
    in (U.UpkeepId `fmap` $(proj 6 0) tuple, pure U.Upkeep <*> (dayToYmd `fmap` a) <*> b <*> c <*> d <*> e)
instance ColumnToRecord (Int, Text, Text, Text, Text) EmployeeMapped where
  convert tuple = (E.EmployeeId $ $(proj 5 0) tuple, uncurryN (const E.Employee) $ tuple)
instance ColumnToRecord (Maybe Int, Maybe Text, Maybe Text, Maybe Text, Maybe Text) MaybeEmployeeMapped where
  convert tuple = let
    maybeE = pure E.Employee <*> $(proj 5 1) tuple <*> $(proj 5 2) tuple <*> $(proj 5 3) tuple <*> $(proj 5 4) tuple
    in (E.EmployeeId `fmap` $(proj 5 0) tuple, maybeE)
instance ColumnToRecord (Int, Text, Int, Int, Bool) UpkeepSequenceMapped where
  convert (a,b,c,d,e) = (MT.MachineTypeId d, US.UpkeepSequence a b c e)
instance ColumnToRecord (Int, Text, Int, Int, Bool, Text, Bool) UpkeepMachineMapped where
  convert (a,b,c,d,e,f,g) = (U.UpkeepId a, M.MachineId c, UM.UpkeepMachine b d e f g)
instance ColumnToRecord (Int, Text, Text) PhotoMetaMapped where
  convert tuple = (P.PhotoId $ $(proj 3 0) tuple, (uncurryN $ const PM.PhotoMeta) tuple)
instance ColumnToRecord (Int, Int, Int, Text) ExtraFieldSettingsMapped where
  convert row = (EF.ExtraFieldId $ $(proj 4 0) row, MK.MachineKindSpecific $ $(proj 4 3) row)
instance ColumnToRecord (Int, Int, Text) ExtraFieldMapped where
  convert tuple = (EF.ExtraFieldId $ $(proj 3 0) tuple, M.MachineId $ $(proj 3 1) tuple, $(proj 3 2) tuple)
instance ColumnToRecord (Int, Day, Text, Maybe Day) TaskMapped where
  convert tuple = let
    (id, startDate, description, endDate) = tuple
    in (T.TaskId id, T.Task (dayToYmd startDate) description (dayToYmd `fmap` endDate))

instance (ColumnToRecord a b) => ColumnToRecord [a] [b] where
  convert rows = fmap convert rows

-- todo rather do two queries
mapUpkeeps :: [((Int, Day, Bool, Text, Text, Text), (Int, Text, Int, Int, Bool, Text, Bool))] 
           -> [(U.UpkeepId, U.Upkeep, [(UM.UpkeepMachine, M.MachineId)])]
mapUpkeeps rows = foldl (\acc (upkeepCols, upkeepMachineCols) ->
  let
    upkeepToAdd = convert upkeepCols :: UpkeepMapped
    upkeepMachineToAdd' = convert upkeepMachineCols :: UpkeepMachineMapped
    upkeepMachineToAdd = (sel3 upkeepMachineToAdd', sel2 upkeepMachineToAdd')
    addUpkeep' = ($(proj 2 0) upkeepToAdd, $(proj 2 1) upkeepToAdd, [upkeepMachineToAdd])
    in case acc of
      [] -> [addUpkeep']
      row : rest | sel1 row == sel1 upkeepToAdd -> 
        $(updateAtN 3 2) (\ums -> upkeepMachineToAdd : ums) row : rest
      _ -> addUpkeep' : acc
  ) [] rows

-- | joins table according with the id in
join :: (Sel1 a DBInt)
     => Query a
     -> QueryArr DBInt a
join tableQuery = proc id' -> do
  table <- tableQuery -< ()
  restrict -< sel1 table .== id'
  returnA -< table

machinePhotosByMachineId :: Int -> Query PhotosMetaTable
machinePhotosByMachineId machineId = proc () -> do
  (photoId, machineId') <- machinePhotosQuery -< ()
  restrict -< (machineId' .== pgInt4 machineId)
  (_, mimeType, fileName) <- join photosMetaQuery -< photoId
  returnA -< (photoId, mimeType, fileName)

machineManufacturersQuery :: String -> Query DBText
machineManufacturersQuery str = autocomplete $ distinct $ proc () -> do
  (_,_,_,manufacturer') <- machineTypesQuery -< ()
  restrict -< (lower manufacturer' `like` (lower $ pgStrictText ("%" <> (pack $ intersperse '%' str) <> "%")))
  returnA -< manufacturer'

otherMachinesInCompanyQuery :: C.CompanyId -> Query MachinesTable
otherMachinesInCompanyQuery (C.CompanyId companyId) = proc () -> do
  machinesRow <- queryTable machinesTable -< ()
  restrict -< (C.CompanyId . pgInt4 $ companyId) .=== _companyFK machinesRow
  returnA -< machinesRow

photoMetaQuery :: Int -> Query PhotosMetaTable
photoMetaQuery photoId = proc () -> do
  result <- join photosMetaQuery -< pgInt4 photoId
  returnA -< result

upkeepSequencesByIdQuery' :: QueryArr DBInt (DBInt, DBText, DBInt, DBInt, DBBool)
upkeepSequencesByIdQuery' = proc (machineTypeId) -> do
  row <- upkeepSequencesQuery -< ()
  restrict -< sel4 row .== machineTypeId
  returnA -< row

upkeepSequencesByIdQuery :: DBInt -> Query (DBInt, DBText, DBInt, DBInt, DBBool)
upkeepSequencesByIdQuery machineTypeId = proc () -> do
  upkeepSequenceRow' <- upkeepSequencesByIdQuery' -< machineTypeId
  returnA -< upkeepSequenceRow'

pastUpkeepMachinesQ :: M.MachineId -> Query UpkeepMachinesTable
pastUpkeepMachinesQ machineId = proc () -> do
  upkeepMachineRow <- upkeepMachinesQuery -< ()
  restrict -< (M.MachineId . $(proj 7 2) $ upkeepMachineRow) .=== fmap pgInt4 machineId
  upkeepRow <- join upkeepsQuery -< $(proj 7 0) upkeepMachineRow
  restrict -< $(proj 6 2) upkeepRow
  returnA -< upkeepMachineRow

like :: Column PGText -> Column PGText -> Column PGBool
like = C.binOp HPQ.OpLike

autocomplete :: (PGOrd a) => Query (Column a) -> Query (Column a)
autocomplete = limit 10 . orderBy (asc id)

machineTypesQuery' :: String -> Query DBText
machineTypesQuery' mid = autocomplete $ proc () -> do
  (_,_,name',_) <- machineTypesQuery -< ()
  restrict -< (lower name' `like` (lower $ pgStrictText ("%" <> (pack $ intersperse '%' mid) <> "%")))
  returnA -< name'

companyByIdQuery :: C.CompanyId -> Query CompanyRead
companyByIdQuery companyId = proc () -> do
  companyRow <- queryTable companiesTable -< ()
  restrict -< (C.getCompanyId (C._companyPK companyRow :: C.CompanyId' DBInt))
    .== (C.getCompanyId (fmap pgInt4 companyId :: C.CompanyId' DBInt))
  returnA -< companyRow

companyByIdCompanyQuery :: C.CompanyId -> Query CompanyCore
companyByIdCompanyQuery companyId = C._companyCore ^<< companyByIdQuery companyId

machineTypesWithCountQuery :: Query (MachineTypesTable, DBInt8)
machineTypesWithCountQuery = let 
  machineTypeIdQ = proc () -> do
    machineRow <- queryTable machinesTable -< ()
    returnA -< _machineTypeFK machineRow
  query' :: Query (MachineTypesTable, Column (Nullable PGInt4))
  query' = leftJoin (queryTable machineTypesTable) machineTypeIdQ $ \(mt, m) -> $(proj 4 0) mt .== m
  aggregatedQuery = AGG.aggregate (p2(p4(AGG.groupBy, AGG.min, AGG.min, AGG.min), AGG.count)) query'
  orderedQuery = orderBy (asc(\( (_,_,_,m),_) -> m) <> asc(\((_,_,name',_),_) -> name')) aggregatedQuery
  in orderedQuery

singleMachineTypeQuery :: Either String Int -> Query (MachineTypesTable, Column PGInt8)
singleMachineTypeQuery machineTypeSid = let
  machineTypeQ = proc () -> do
    machineTypeNameRow @ (mtId',_,name',_) <- machineTypesQuery -< ()
    restrict -< case machineTypeSid of
      Right(machineTypeId) -> (mtId' .== pgInt4 machineTypeId)
      Left(machineTypeName) -> (name' .== pgString machineTypeName)
    returnA -< machineTypeNameRow
  machineTIdQ = proc () -> do
    machineRow <- queryTable machinesTable -< ()
    returnA -< _machineTypeFK machineRow
  withMachinesQ :: Query (MachineTypesTable, Column (Nullable PGInt4))
  withMachinesQ = leftJoin machineTypeQ machineTIdQ $ \(mt, m) -> 
    $(proj 4 0) mt .== m
  essentialWithMQ = proc () -> do
    (mt, m) <- withMachinesQ -< ()
    returnA -< (mt, m)
  withCountQ = AGG.aggregate aggregator essentialWithMQ where
    aggregator = p2 (p4 (AGG.min, AGG.min, AGG.min, AGG.min), AGG.count)
  in withCountQ

machinesInUpkeepQuery''' :: Int -> Query (MachinesTable, MachineTypesTable, UpkeepMachinesTable)
machinesInUpkeepQuery''' upkeepId = proc () -> do
  upkeepMachineRow <- machinesInUpkeepQuery upkeepId -< ()
  machineRow <- queryTable machinesTable -< ()
  restrict -< _machinePK machineRow .=== (M.MachineId . $(proj 7 2) $ upkeepMachineRow)
  machineTypeRow <- join machineTypesQuery -< _machineTypeFK machineRow
  returnA -< (machineRow, machineTypeRow, upkeepMachineRow)

machinesInUpkeepQuery'' :: U.UpkeepId -> Query (MachinesTable, MachineTypesTable, ContactPersonsLeftJoinTable, UpkeepMachinesTable)
machinesInUpkeepQuery'' (U.UpkeepId upkeepIdInt) = let
  joined = leftJoin
    (machinesInUpkeepQuery''' upkeepIdInt)
    contactPersonsQuery
    (\((machineRow,_,_), contactPersonRow) -> _machinePK machineRow .=== 
      (M.MachineId . $(proj 5 1) $ contactPersonRow))
  in proc () -> do
    ((a,b,c), contactPersons) <- joined -< ()
    returnA -< (a,b,contactPersons,c)
  
machinesQ :: C.CompanyId -> Query (MachinesTable, MachineTypesTable)
machinesQ companyId = orderBy (asc(\(machine, _) -> C.getCompanyId . _companyFK $ machine)) $ proc () -> do
  machineRow <- queryTable machinesTable -< ()
  mt <- join machineTypesQuery -< _machineTypeFK machineRow
  restrict -< fmap pgInt4 companyId .=== _companyFK machineRow
  returnA -< (machineRow, mt)

machinesInCompanyQuery :: 
  C.CompanyId -> 
  Query (MachinesTable, MachineTypesTable, ContactPersonsLeftJoinTable, UpkeepsLeftJoinTable)
machinesInCompanyQuery companyId = let
  machinesContactPersonsQ = leftJoin 
    (machinesQ companyId)
    contactPersonsQuery 
    (\((machineRow,_), cpRow) -> _contactPersonFK machineRow .== (maybeToNullable $ Just $ sel1 cpRow))
  lastUpkeepForMachineQ = AGG.aggregate (p2(AGG.max, AGG.groupBy)) $ proc () -> do
    umRow <- queryTable upkeepMachinesTable -< ()
    uRow <- join . queryTable $ upkeepsTable -< $(proj 7 0) umRow
    returnA -< ($(proj 6 1) uRow, $(proj 7 2) umRow)
  lastUpkeepsQ = proc () -> do
    (upkeepDate, upkeepMachineFK) <- lastUpkeepForMachineQ -< ()
    umRow <- queryTable upkeepMachinesTable -< ()
    restrict -< $(proj 7 2) umRow .=== upkeepMachineFK
    upkeepRow <- join . queryTable $ upkeepsTable -< $(proj 7 0) umRow
    restrict -< $(proj 6 1) upkeepRow .== upkeepDate
    returnA -< (upkeepRow, upkeepMachineFK)
  withLastUpkeep = leftJoin
    machinesContactPersonsQ
    lastUpkeepsQ
    (\(machine, upkeep) -> (_machinePK . fst . fst $ machine) .=== (M.MachineId . snd $ upkeep))
  in proc () -> do
    (((a,b),c),(d,_::MBInt)) <- withLastUpkeep -< ()
    returnA -< (a,b,c,d)

machinesInCompanyQuery' :: U.UpkeepId -> Query (MachinesTable, MachineTypesTable)
machinesInCompanyQuery' (U.UpkeepId upkeepId) = let
  companyPKQ = distinct $ proc () -> do
    upkeepMachineRow <- join upkeepMachinesQuery -< pgInt4 upkeepId
    machineRow <- queryTable machinesTable -< ()
    restrict -< (M.MachineId . $(proj 7 2) $ upkeepMachineRow) .=== _machinePK machineRow
    companyRow <- queryTable companiesTable -< ()
    restrict -< C._companyPK companyRow .=== _companyFK machineRow
    returnA -< C._companyPK companyRow
  in proc () -> do
    companyId <- companyPKQ -< ()
    machineRow <- queryTable machinesTable -< ()
    restrict -< _companyFK machineRow .=== companyId
    machineTypeRow <- join . queryTable $ machineTypesTable -< _machineTypeFK machineRow
    returnA -< (machineRow, machineTypeRow)

companyUpkeepsQuery :: Int -> Query UpkeepsTable
companyUpkeepsQuery companyId = let 
  upkeepsQuery' = proc () -> do
    (upkeepFK,_,machineFK,_,_,_,_) <- upkeepMachinesQuery -< ()
    machineRow <- queryTable machinesTable -< ()
    restrict -< _machinePK machineRow .=== M.MachineId machineFK
    upkeep @ (_,_,closed,_,_,_) <- join upkeepsQuery -< upkeepFK
    restrict -< (closed .== pgBool True)
    restrict -< (_companyFK machineRow .=== (C.CompanyId . pgInt4 $ companyId))
    returnA -< upkeep
  aggregatedUpkeepsQuery = AGG.aggregate (p6(AGG.groupBy, AGG.min, AGG.boolOr,
    AGG.min, AGG.min, AGG.min)) upkeepsQuery'
  orderedUpkeepQuery = orderBy (asc(\(_,date,_,_,_,_) -> date)) $ aggregatedUpkeepsQuery
  in orderedUpkeepQuery

-- | query, that returns expanded machine type, not just the id
expandedMachinesQuery :: Maybe Int -> Query (MachinesTable, MachineTypesTable)
expandedMachinesQuery machineId = proc () -> do
  machineRow <- queryTable machinesTable -< ()
  machineTypesRow <- join machineTypesQuery -< _machineTypeFK machineRow
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

machinesInCompanyByUpkeepQuery :: Int -> Query (DBInt, MachinesTable, MachineTypesTable)
machinesInCompanyByUpkeepQuery upkeepId = let
  companyPKQuery = limit 1 $ proc () -> do
    upkeepMachineRow <- join upkeepMachinesQuery -< pgInt4 upkeepId
    machineRow <- queryTable machinesTable -< ()
    restrict -< (M.MachineId . $(proj 7 2) $ upkeepMachineRow) .=== _machinePK machineRow
    returnA -< _companyFK machineRow
  in proc () -> do
    companyPK <- companyPKQuery -< ()
    machineRow <- queryTable machinesTable -< ()
    restrict -< _companyFK machineRow .=== companyPK
    mt <- join machineTypesQuery -< _machineTypeFK machineRow
    returnA -< (C.getCompanyId companyPK, machineRow, mt)

expandedUpkeepsQuery2 :: Int -> Query (UpkeepsTable, UpkeepMachinesTable)
expandedUpkeepsQuery2 upkeepId = proc () -> do
  upkeepRow <- join upkeepsQuery -< pgInt4 upkeepId
  upkeepMachineRow <- join upkeepMachinesQuery -< pgInt4 upkeepId
  returnA -< (upkeepRow, upkeepMachineRow)

expandedUpkeepsQuery :: Query (UpkeepsTable, UpkeepMachinesTable)
expandedUpkeepsQuery = proc () -> do
  upkeepRow @ (upkeepPK,_,_,_,_,_) <- upkeepsQuery -< ()
  upkeepMachineRow <- join upkeepMachinesQuery -< upkeepPK
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

joinMachine :: QueryArr MachinePK MachinesTable
joinMachine = proc machinePK -> do
  machineRow <- queryTable machinesTable -< ()
  restrict -< _machinePK machineRow .=== machinePK
  returnA -< machineRow

nextServiceUpkeepsQuery :: Int -> Query (UpkeepsTable, UpkeepMachinesTable)
nextServiceUpkeepsQuery machineId = proc () -> do
  machineRow <- joinMachine -< M.MachineId . pgInt4 $ machineId
  upkeepMachineRow <- upkeepMachinesQuery -< ()
  restrict -< (M.MachineId . sel3 $ upkeepMachineRow) .=== _machinePK machineRow
  upkeepRow <- join upkeepsQuery -< sel1 upkeepMachineRow
  returnA -< (upkeepRow, upkeepMachineRow)

upkeepsDataForMachine :: M.MachineId -> Query (UpkeepsTable, UpkeepMachinesTable, EmployeeLeftJoinTable)
upkeepsDataForMachine (M.MachineId machineId) = let 
  upkeepEQ :: Query (UpkeepEmployeesTable, EmployeeTable)
  upkeepEQ = proc () -> do
    upkeepEmployeeRow <- queryTable upkeepEmployeesTable -< ()
    employeeRow <- join . queryTable $ employeesTable -< $(proj 3 1) upkeepEmployeeRow
    returnA -< (upkeepEmployeeRow, employeeRow)
  upkeepQ :: Query (UpkeepsTable, UpkeepMachinesTable)
  upkeepQ = proc () -> do
    machineRow <- queryTable machinesTable -< ()
    restrict -< _machinePK machineRow .=== (M.MachineId . pgInt4 $ machineId)
    upkeepMachineRow <- upkeepMachinesQuery -< ()
    restrict -< (M.MachineId . sel3 $ upkeepMachineRow) .=== _machinePK machineRow
    upkeepRow <- join upkeepsQuery -< sel1 upkeepMachineRow
    returnA -< (upkeepRow, upkeepMachineRow)
  ljQ :: Query ((UpkeepsTable, UpkeepMachinesTable), (
    (Column (Nullable PGInt4), Column (Nullable PGInt4), Column (Nullable PGInt4)), EmployeeLeftJoinTable))
  ljQ = leftJoin upkeepQ upkeepEQ (\((u, _), (ue, _)) -> $(proj 6 0) u .== $(proj 3 0) ue)
  in proc () -> do
    ((u, um), (_, e)) <- ljQ -< ()
    returnA -< (u, um, e)

nextServiceUpkeepSequencesQuery :: M.MachineId -> Query (DBInt, DBText, DBInt, DBInt, DBBool)
nextServiceUpkeepSequencesQuery (M.MachineId machineId) = proc () -> do
  machineRow <- joinMachine -< M.MachineId . pgInt4 $ machineId
  machineTypeRow <- machineTypesQuery -< ()
  restrict -< sel1 machineTypeRow .== _machineTypeFK machineRow
  upkeepSequenceRow <- upkeepSequencesByIdQuery' -< sel1 machineTypeRow
  returnA -< upkeepSequenceRow

expandedUpkeepsByCompanyQuery :: C.CompanyId -> Query 
  (UpkeepsTable, UpkeepMachinesTable, MachinesTable, MachineTypesTable)
expandedUpkeepsByCompanyQuery companyId = let
  upkeepsWithMachines = proc () -> do
    upkeepRow @ (upkeepPK,_,_,_,_,_) <- upkeepsQuery -< ()
    upkeepMachineRow <- join upkeepMachinesQuery -< upkeepPK
    machine <- joinMachine -< M.MachineId . $(proj 7 2) $ upkeepMachineRow
    machineType <- join machineTypesQuery -< _machineTypeFK machine
    restrict -< _companyFK machine .=== fmap pgInt4 companyId
    returnA -< (upkeepRow, upkeepMachineRow, machine, machineType)
  orderedUpkeepsWithMachines = orderBy (desc (\(u,_,_,_) -> $(proj 6 1) u) <> desc (\(u,_,_,_) -> $(proj 6 0) u)) upkeepsWithMachines
  in orderedUpkeepsWithMachines

singleEmployeeQuery :: Int -> Query (EmployeeTable)
singleEmployeeQuery employeeId = proc () -> do
  employeeRow <- join employeesQuery -< (pgInt4 employeeId)
  returnA -< employeeRow

employeesInUpkeeps :: [U.UpkeepId] -> Query (DBInt, EmployeeTable)
employeesInUpkeeps upkeepIds = proc () -> do
  upkeepRow <- upkeepsQuery -< ()
  restrict -< in_ ((pgInt4 . U.getUpkeepId) `fmap` upkeepIds) ($(proj 6 0) upkeepRow)
  upkeepEmployeeRow <- join . queryTable $ upkeepEmployeesTable -< $(proj 6 0) upkeepRow
  employeeRow <- join . queryTable $ employeesTable -< $(proj 3 1) upkeepEmployeeRow
  returnA -< ($(proj 6 0) upkeepRow, employeeRow)

groupedPlannedUpkeepsQuery :: Query (UpkeepsTable, DBInt, (C.CompanyId' DBInt, CompanyCore))
groupedPlannedUpkeepsQuery = let
  plannedUpkeepsQuery = proc () -> do
    upkeepRow @ (upkeepPK,_,upkeepClosed,_,_,_) <- upkeepsQuery -< ()
    restrict -< upkeepClosed .== pgBool False
    upkeepMachinesRow <- join upkeepMachinesQuery -< upkeepPK
    machineRow <- joinMachine -< M.MachineId . $(proj 7 2) $ upkeepMachinesRow
    companyRow <- queryTable companiesTable -< ()
    machineTypeRow <- join . queryTable $ machineTypesTable -< _machineTypeFK machineRow
    restrict -< C._companyPK companyRow .=== _companyFK machineRow
    returnA -< (upkeepRow, $(proj 4 1) machineTypeRow , (C._companyPK companyRow, C._companyCore companyRow))
  in orderBy (asc(\((_,date,_,_,_,_), _, _) -> date)) $ 
    AGG.aggregate (p3 (p6(AGG.groupBy, AGG.min, AGG.boolOr, AGG.min, AGG.min, AGG.min), AGG.min,
      p2(pCompanyId . C.CompanyId $ AGG.min, pCompany $ C.Company AGG.min AGG.min AGG.min))) 
        plannedUpkeepsQuery

singleContactPersonQuery :: Int -> Query (ContactPersonsTable, CompanyRead)
singleContactPersonQuery contactPersonId = proc () -> do
  contactPersonRow <- join contactPersonsQuery -< pgInt4 contactPersonId
  companyRow <- queryTable companiesTable -< ()
  restrict -< (C.getCompanyId . C._companyPK) companyRow .== $(proj 5 1) contactPersonRow
  returnA -< (contactPersonRow, companyRow)

machinesInUpkeepQuery :: Int -> Query UpkeepMachinesTable
machinesInUpkeepQuery upkeepId = proc () -> do
  upkeepMachine <- join upkeepMachinesQuery -< pgInt4 upkeepId
  returnA -< upkeepMachine

extraFieldsPerKindQuery :: Int -> Query ExtraFieldSettingsTable
extraFieldsPerKindQuery machineKind = orderBy (asc $ $(proj 4 2)) $ proc () -> do
  extraFieldRow <- extraFieldSettingsQuery -< ()
  restrict -< pgInt4 machineKind .== $(proj 4 1) extraFieldRow
  returnA -< extraFieldRow

machineIdsHavingKind :: Int -> Query MachinePK
machineIdsHavingKind machineTypeKind = proc () -> do
  machineTypeRow <- machineTypesQuery -< ()
  restrict -< pgInt4 machineTypeKind .== $(proj 4 1) machineTypeRow
  machineRow <- queryTable machinesTable -< ()
  restrict -< $(proj 4 0) machineTypeRow .== _machineTypeFK machineRow
  returnA -< _machinePK machineRow

employeeIdsInUpkeep :: Int -> Query DBInt
employeeIdsInUpkeep upkeepId = proc () -> do
  (_, employeeId, _) <- join . queryTable $ upkeepEmployeesTable -< pgInt4 upkeepId
  returnA -< employeeId

employeesInUpkeep :: Int -> Query EmployeeTable
employeesInUpkeep upkeepId = proc () -> do
  employeeId <- employeeIdsInUpkeep upkeepId -< ()
  employeeRow <- join employeesQuery -< employeeId
  returnA -< employeeRow

extraFieldsForMachineQuery :: M.MachineId -> Query (ExtraFieldsTable, ExtraFieldSettingsTable)
extraFieldsForMachineQuery (M.MachineId machineId) = orderBy (asc $ $(proj 4 2) . snd) $ proc () -> do
  machineRow <- joinMachine -< (M.MachineId . pgInt4 $ machineId)
  extraFieldRow <- extraFieldsQuery -< ()
  restrict -< (M.MachineId . $(proj 3 1) $ extraFieldRow) .=== _machinePK machineRow
  extraFieldSettingRow <- join extraFieldSettingsQuery -< $(proj 3 0) extraFieldRow
  returnA -< (extraFieldRow, extraFieldSettingRow)

mainEmployeesInDayQ :: Day
                    -> Query EmployeeTable
mainEmployeesInDayQ day = distinct $ proc () -> do
  upkeepRow <- upkeepsQuery -< ()
  restrict -< $(proj 6 1) upkeepRow .== pgDay day
  upkeepEmployeeRow <- join . queryTable $ upkeepEmployeesTable -< $(proj 6 0) upkeepRow
  restrict -< pgInt4 0 .== $(proj 3 2) upkeepEmployeeRow
  employeeRow <- join employeesQuery -< $(proj 3 1) upkeepEmployeeRow
  returnA -< employeeRow

dailyPlanQuery :: Maybe E.EmployeeId -> Day -> Query (UpkeepsTable, Column (PGArray PGInt4))
dailyPlanQuery employeeId' day = let
  q = proc () -> do
    upkeepRow <- upkeepsQuery -< ()
    restrict -< $(proj 6 1) upkeepRow .== pgDay day
    upkeepEmployeeRow <- join . queryTable $ upkeepEmployeesTable -< $(proj 6 0) upkeepRow
    restrict -< case employeeId' of
      Just (E.EmployeeId employeeId) -> pgInt4 employeeId .== $(proj 3 1) upkeepEmployeeRow
      Nothing -> pgBool False -- todo return no results earlier than here in db query
    restrict -< pgInt4 0 .== $(proj 3 2) upkeepEmployeeRow
    upkeepEmployeeRowData <- join . queryTable $ upkeepEmployeesTable -< $(proj 6 0) upkeepRow
    returnA -< (upkeepRow, $(proj 3 1) upkeepEmployeeRowData)
  in AGG.aggregate (p2(p6(AGG.groupBy, AGG.min, AGG.boolOr, AGG.min, AGG.min, AGG.min), p1(aggrArray))) q

tasksForEmployeeQuery :: E.EmployeeId -> Query TasksTable
tasksForEmployeeQuery (E.EmployeeId employeeId) = proc () -> do
  taskRow <- queryTable tasksTable -< ()
  taskEmployeeRow <- join . queryTable $ taskEmployeesTable -< $(proj 4 0) taskRow
  restrict -< pgInt4 employeeId .== $(proj 2 1) taskEmployeeRow
  returnA -< taskRow

getTaskQuery :: T.TaskId -> Query TasksTable
getTaskQuery (T.TaskId taskIdInt) = proc () -> do
  taskRow <- join . queryTable $ tasksTable -< pgInt4 taskIdInt
  returnA -< taskRow

lastRecommendationQuery :: C.CompanyId -> Query UpkeepsTable
lastRecommendationQuery (C.CompanyId companyId) = let
  latestUpkeepQ = limit 1 . orderBy (desc $(proj 6 1)) $ proc () -> do
    machineRow <- queryTable machinesTable -< ()
    restrict -< _companyFK machineRow .=== (C.CompanyId . pgInt4 $ companyId)
    upkeepMachineRow <- queryTable upkeepMachinesTable -< ()
    restrict -< (M.MachineId . $(proj 7 2) $ upkeepMachineRow) .=== _machinePK machineRow
    upkeepRow <- join . queryTable $ upkeepsTable -< $(proj 7 0) upkeepMachineRow
    restrict -< $(proj 6 2) upkeepRow .== pgBool True
    returnA -< upkeepRow
  in latestUpkeepQ

takenColoursQuery :: Query DBText
takenColoursQuery = distinct $ proc () -> do
  employeeRow <- queryTable employeesTable -< ()
  returnA -< $(proj 5 4) employeeRow

aggrArray :: AGG.Aggregator (Column a) (Column (PGArray a))
aggrArray = IAGG.makeAggr . HPQ.AggrOther $ "array_agg"

notesForUpkeep :: Int -> Query (DBInt, DBText, DBText)
notesForUpkeep upkeepId = proc () -> do
  upkeepMachinesRow <- join upkeepMachinesQuery -< pgInt4 upkeepId
  machinesRow <- joinMachine -< (M.MachineId . $(proj 7 2) $ upkeepMachinesRow)
  machineTypesRow <- join machineTypesQuery -< _machineTypeFK machinesRow
  returnA -< ($(proj 7 2) upkeepMachinesRow, $(proj 4 2) machineTypesRow, $(proj 7 1) upkeepMachinesRow)

multiEmployeeQuery :: [Int] -> Query EmployeeTable
multiEmployeeQuery employeeIds = proc () -> do
  employeeRow <- employeesQuery -< ()
  restrict -< in_ (pgInt4 `fmap` employeeIds) ($(proj 5 0) employeeRow)
  returnA -< employeeRow

photosInUpkeepQuery :: U.UpkeepId -> Query DBInt
photosInUpkeepQuery (U.UpkeepId upkeepId) = proc () -> do
  upkeepPhotosRow <- queryTable upkeepPhotosTable -< ()
  restrict -< $(proj 2 1) upkeepPhotosRow .== pgInt4 upkeepId
  returnA -< $(proj 2 0) upkeepPhotosRow

companyInUpkeepQuery :: U.UpkeepId -> Query CompanyCore
companyInUpkeepQuery (U.UpkeepId upkeepIdInt) = distinct $ proc () -> do
  upkeepMachineRow <- join upkeepMachinesQuery -< pgInt4 upkeepIdInt
  machineRow <- joinMachine -< (M.MachineId . $(proj 7 2) $ upkeepMachineRow)
  companyRow <- queryTable companiesTable -< ()
  restrict -< C._companyPK companyRow .=== _companyFK machineRow
  returnA -< C._companyCore companyRow

runMachinesInCompanyQuery :: C.CompanyId -> Connection -> 
  IO [(M.MachineId, M.Machine, C.CompanyId, MT.MachineTypeId, 
    MT.MachineType, Maybe CP.ContactPerson, M.MachineId' (Maybe Int), Maybe U.Upkeep)]
runMachinesInCompanyQuery companyId connection = do
  rows <- runQuery connection (machinesInCompanyQuery companyId)
  let 
    mapRow row = let
      (machineType :: MachineTypeMapped, contactPerson :: MaybeContactPersonMapped, 
          upkeepMapped :: MaybeUpkeepMapped) = 
        (convert . (\(_,x,_,_) -> x) $ row, 
          convert . (\(_,_,x,_) -> x) $ row,
          convert . (\(_,_,_,x) -> x) $ row)
      (machineRecord :: MachineRecord) = 
        over (machine . M.operationStartDateL) (fmap dayToYmd) $ (\(x,_,_,_) -> x) row
      in (_machinePK machineRecord, _machine machineRecord, _companyFK machineRecord, sel1 machineType, 
        sel2 machineType, sel3 contactPerson, _linkageFK machineRecord, $(proj 2 1) upkeepMapped)
  return $ fmap mapRow rows

runExpandedMachinesQuery' :: Maybe Int -> Connection 
  -> IO [(MachineRecord, (Int, Int, Text, Text))]
runExpandedMachinesQuery' machineId connection = do
  rows <- runQuery connection (expandedMachinesQuery machineId)
  let rowsMapped = over (mapped . _1 . machine . M.operationStartDateL . mapped) dayToYmd rows
  return rowsMapped

runCompanyUpkeepsQuery :: Int -> Connection -> 
  IO [(Int, Day, Bool, Text, Text, Text)]
runCompanyUpkeepsQuery companyId connection = 
  runQuery connection (companyUpkeepsQuery companyId)

convertExpanded :: 
  (MachineRecord, (Int, Int, Text, Text)) -> 
  (M.MachineId, M.Machine, C.CompanyId, MT.MachineTypeId, MT.MachineType)
convertExpanded (row1, row2) = let 
  (mt :: MachineTypeMapped) = convert row2
  (m :: MachineRecord) = row1
  in (_machinePK m, _machine m, _companyFK m, sel1 mt, sel2 mt)

runExpandedMachinesQuery :: Maybe Int -> Connection -> IO[(M.MachineId, M.Machine, C.CompanyId, MT.MachineTypeId, MT.MachineType)]
runExpandedMachinesQuery machineId connection = do
  rows <- runExpandedMachinesQuery' machineId connection
  return $ fmap convertExpanded rows

runMachineTypesQuery' :: String -> Connection -> IO[Text]
runMachineTypesQuery' mid connection = runQuery connection (machineTypesQuery' mid)

runMachinesInCompanyByUpkeepQuery :: Int -> Connection -> 
  IO[(C.CompanyId, (M.MachineId, M.Machine, C.CompanyId, MT.MachineTypeId, MT.MachineType))]
runMachinesInCompanyByUpkeepQuery upkeepId connection = do
  rows <- runQuery connection (machinesInCompanyByUpkeepQuery upkeepId)
  return $ map (\(companyId,a,b) -> (C.CompanyId companyId, convertExpanded (mapMachineDate a,b))) rows 

mapMachineDate ::
  MachineRecord' ->
  MachineRecord
mapMachineDate machinesTable = over (machine . M.operationStartDateL) (fmap dayToYmd) machinesTable

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

deletePhoto :: Connection
            -> Int
            -> IO ()
deletePhoto connection photoId = do
  let q = " delete from photos where id = ? "
  _ <- execute connection q (Only photoId)
  return ()

singleRowOrColumn :: Monad m
                  => [a] 
                  -> ExceptT (Reason r) m a
singleRowOrColumn result = case result of
  row : xs | null xs -> return row
  [] -> throwError $ InputError $ ParseError "no record"
  _ -> throwError $ InputError $ ParseError "more than one record failure"
