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
  compressorsTable ,
  dryersTable ,
  -- basic queries
  companiesQuery ,
  machinesQuery ,
  machineTypesQuery ,
  upkeepsQuery ,
  upkeepMachinesQuery ,
  employeesQuery ,
  upkeepSequencesQuery ,
  machinePhotosQuery ,
  getMachinePhoto ,
  singleEmployeeQuery ,
  contactPersonsQuery ,
  compressorsQuery ,
  dryersQuery ,
  -- manipulations
  addCompany ,
  addMachinePhoto ,
  -- runs
  runExpandedMachinesQuery ,
  runMachinesInCompanyQuery ,
  runMachineTypesQuery' ,
  runMachinesInCompanyByUpkeepQuery ,
  runCompanyUpkeepsQuery ,
  -- more complex query
  machineSpecificQuery ,
  expandedUpkeepsQuery2 ,
  groupedPlannedUpkeepsQuery ,
  expandedUpkeepsQuery ,
  companyByIdQuery ,
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
  -- helpers
  withConnection ,
  singleRowOrColumn ,
  mapUpkeeps ,
  -- mappings
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
  UpkeepSequenceMapped ,
  CompressorMapped ,
  DryerMapped ,
  UpkeepMachineMapped ,
  PhotoMetaMapped ,
  MachineMapped ) where

import Database.PostgreSQL.Simple (ConnectInfo(..), Connection, defaultConnectInfo, connect, close, query,
  Only(..), Binary(..))

import Opaleye.QueryArr (Query, QueryArr)
import Opaleye.Table (Table(Table), required, queryTable, optional)
import Opaleye.Column (Column, Nullable)
import Opaleye.Order (orderBy, asc, limit)
import Opaleye.RunQuery (runQuery)
import Opaleye.Operators (restrict, lower, (.==))
import Opaleye.PGTypes (pgInt4, PGDate, PGBool, PGInt4, PGInt8, PGText, pgString, pgBool, PGFloat8, pgDouble)
import Opaleye.Manipulation (runInsertReturning)
import qualified Opaleye.Aggregate as AGG
import Opaleye.Join (leftJoin)
import Opaleye.Distinct (distinct)

import Control.Monad.Error.Class (throwError)
import Control.Arrow (returnA)
import Control.Applicative ((<*>), pure)
import Control.Monad.Trans.Except (ExceptT)

import Data.Profunctor.Product (p1, p2, p3, p4, p5, p6, p7, p10)
import Data.Time.Calendar (Day)
import Data.List (intersperse)
import Data.Tuple.All (Sel1, sel1, sel2, sel3, sel4, uncurryN, sel5, upd5, upd2, upd4)
import Data.ByteString.Lazy (ByteString)

import Rest.Types.Error (DataError(ParseError), Reason(InputError))

import qualified Crm.Shared.Company as C
import qualified Crm.Shared.Employee as E
import qualified Crm.Shared.ContactPerson as CP
import qualified Crm.Shared.Machine as M
import qualified Crm.Shared.Dryer as MD
import qualified Crm.Shared.Compressor as MC
import qualified Crm.Shared.MachineType as MT
import qualified Crm.Shared.MachineKind as MK
import qualified Crm.Shared.Upkeep as U
import qualified Crm.Shared.UpkeepSequence as US
import qualified Crm.Shared.UpkeepMachine as UM
import qualified Crm.Shared.PhotoMeta as PM
import qualified Crm.Shared.Photo as P

import Crm.Server.Helpers (dayToYmd, maybeToNullable)

import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ
import qualified Opaleye.Internal.Column as C

type DBInt = Column PGInt4
type DBInt8 = Column PGInt8
type DBText = Column PGText
type DBDate = Column PGDate
type DBBool = Column PGBool

type CompaniesTable = (DBInt, DBText, DBText, DBText, Column (Nullable PGFloat8), Column (Nullable PGFloat8))
type CompaniesWriteTable = (Maybe DBInt, DBText, DBText, DBText, Column (Nullable PGFloat8), Column (Nullable PGFloat8))

type ContactPersonsTable = (DBInt, DBInt, DBText, DBText, DBText)
type ContactPersonsLeftJoinTable = (Column (Nullable PGInt4), Column (Nullable PGInt4), 
  Column (Nullable PGText), Column (Nullable PGText), Column (Nullable PGText))
type ContactPersonsWriteTable = (Maybe DBInt, DBInt, DBText, DBText, DBText)

type CompressorsTable = (DBInt, DBText)

type DryersTable = (DBInt, DBText)

type MachinesTable = (DBInt, DBInt, Column (Nullable PGInt4) , DBInt, Column (Nullable PGDate), DBInt, DBInt, DBText, DBText, DBText)
type MachinesWriteTable = (Maybe DBInt, DBInt, Column (Nullable PGInt4), DBInt, 
  Column (Nullable PGDate), DBInt, DBInt, DBText, DBText, DBText)

type MachineTypesTable = (DBInt, DBInt, DBText, DBText)
type MachineTypesWriteTable = (Maybe DBInt, DBInt, DBText, DBText)

type UpkeepsTable = (DBInt, DBDate, DBBool, Column (Nullable PGInt4), DBText, DBText, DBText)
type UpkeepsWriteTable = (Maybe DBInt, DBDate, DBBool, Column (Nullable PGInt4), DBText, DBText, DBText)

type UpkeepMachinesTable = (DBInt, DBText, DBInt, DBInt, DBBool)

type EmployeeTable = (DBInt, DBText, DBText, DBText)
type EmployeeLeftJoinTable = (Column (Nullable PGInt4), Column (Nullable PGText), Column (Nullable PGText), Column (Nullable PGText))
type EmployeeWriteTable = (Maybe DBInt, DBText, DBText, DBText)

type UpkeepSequencesTable = (DBInt, DBText, DBInt, DBInt, DBBool)

type PhotosMetaTable = (DBInt, DBText, DBText)

type MachinePhotosTable = (DBInt, DBInt)

photosMetaTable :: Table PhotosMetaTable PhotosMetaTable
photosMetaTable = Table "photos_meta" $ p3 (
  required "photo_id" ,
  required "mime_type" ,
  required "file_name" )

machinePhotosTable :: Table MachinePhotosTable MachinePhotosTable
machinePhotosTable = Table "machine_photos" $ p2 (
  required "photo_id" ,
  required "machine_id" )

companiesTable :: Table CompaniesWriteTable CompaniesTable
companiesTable = Table "companies" $ p6 (
  optional "id" ,
  required "name" ,
  required "plant" ,
  required "address" ,
  required "latitude" ,
  required "longitude" )

contactPersonsTable :: Table ContactPersonsWriteTable ContactPersonsTable
contactPersonsTable = Table "contact_persons" $ p5 (
  optional "id" ,
  required "company_id" ,
  required "name" ,
  required "phone" ,
  required "position" )

compressorsTable :: Table CompressorsTable CompressorsTable
compressorsTable = Table "compressors" $ p2 (
  required "machine_id" ,
  required "note" )

dryersTable :: Table DryersTable DryersTable
dryersTable = Table "dryers" $ p2 (
  required "machine_id" ,
  required "note" )

machinesTable :: Table MachinesWriteTable MachinesTable
machinesTable = Table "machines" $ p10 (
  optional "id" ,
  required "company_id" ,
  required "contact_person_id" ,
  required "machine_type_id" ,
  required "operation_start" ,
  required "initial_mileage" ,
  required "mileage_per_year" ,
  required "note" ,
  required "serial_number" ,
  required "year_of_manufacture" )

machineTypesTable :: Table MachineTypesWriteTable MachineTypesTable
machineTypesTable = Table "machine_types" $ p4 (
  optional "id" ,
  required "machine_kind" ,
  required "name" ,
  required "manufacturer" )

upkeepsTable :: Table UpkeepsWriteTable UpkeepsTable
upkeepsTable = Table "upkeeps" $ p7 (
  optional "id" ,
  required "date_" ,
  required "closed" ,
  required "employee_id" ,
  required "work_hours" ,
  required "work_description" ,
  required "recommendation" )

upkeepMachinesTable :: Table UpkeepMachinesTable UpkeepMachinesTable
upkeepMachinesTable = Table "upkeep_machines" $ p5 (
  required "upkeep_id" ,
  required "note" ,
  required "machine_id" ,
  required "recorded_mileage" ,
  required "warranty" )

employeesTable :: Table EmployeeWriteTable EmployeeTable
employeesTable = Table "employees" $ p4 (
  optional "id" ,
  required "name" ,
  required "contact" ,
  required "capabilities" )

upkeepSequencesTable :: Table UpkeepSequencesTable UpkeepSequencesTable
upkeepSequencesTable = Table "upkeep_sequences" $ p5 (
  required "display_ordering" ,
  required "label" ,
  required "repetition" ,
  required "machine_type_id" ,
  required "one_time" )

dryersQuery :: Query DryersTable
dryersQuery = queryTable dryersTable

compressorsQuery :: Query CompressorsTable
compressorsQuery = queryTable compressorsTable

contactPersonsQuery :: Query ContactPersonsTable
contactPersonsQuery = queryTable contactPersonsTable

photosMetaQuery :: Query PhotosMetaTable
photosMetaQuery = queryTable photosMetaTable

machinePhotosQuery :: Query MachinePhotosTable
machinePhotosQuery = queryTable machinePhotosTable

companiesQuery :: Query CompaniesTable
companiesQuery = queryTable companiesTable

machinesQuery :: Query MachinesTable
machinesQuery = queryTable machinesTable

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

type MachineMapped = (M.MachineId, C.CompanyId, Maybe CP.ContactPersonId, MT.MachineTypeId, M.Machine)
type CompanyMapped = (C.CompanyId, C.Company)
type MachineTypeMapped = (MT.MachineTypeId, MT.MachineType)
type ContactPersonMapped = (CP.ContactPersonId, C.CompanyId, CP.ContactPerson)
type MaybeContactPersonMapped = (Maybe CP.ContactPersonId, Maybe C.CompanyId, Maybe CP.ContactPerson)
type MaybeEmployeeMapped = (Maybe E.EmployeeId, Maybe E.Employee)
type UpkeepMapped = (U.UpkeepId, Maybe E.EmployeeId, U.Upkeep)
type EmployeeMapped = (E.EmployeeId, E.Employee)
type UpkeepSequenceMapped = (MT.MachineTypeId, US.UpkeepSequence)
type CompressorMapped = (M.MachineId, MC.Compressor)
type DryerMapped = (M.MachineId, MD.Dryer)
type UpkeepMachineMapped = (U.UpkeepId, M.MachineId, UM.UpkeepMachine)
type PhotoMetaMapped = (P.PhotoId, PM.PhotoMeta)

instance ColumnToRecord (Int, String, String, String, Maybe Double, Maybe Double) CompanyMapped where
  convert tuple = let 
    company = (uncurryN $ const ((fmap . fmap . fmap) (const . const) C.Company)) tuple
    in (C.CompanyId $ sel1 tuple, company)
instance ColumnToRecord 
    (Int, Int, Maybe Int, Int, Maybe Day, Int, Int, String, String, String) 
    MachineMapped where
  convert tuple = let
    machineTuple = upd5 (fmap dayToYmd $ sel5 tuple) tuple
    in (M.MachineId $ sel1 tuple, C.CompanyId $ sel2 tuple, CP.ContactPersonId `fmap` sel3 tuple, 
      MT.MachineTypeId $ sel4 tuple, (uncurryN $ const $ const $ const $ const M.Machine) machineTuple)
instance ColumnToRecord (Int, Int, String, String) MachineTypeMapped where
  convert tuple = (MT.MachineTypeId $ sel1 tuple, (uncurryN $ const MT.MachineType) 
    (upd2 (MK.dbReprToKind $ sel2 tuple) tuple))
instance ColumnToRecord (Int, Int, String, String, String) ContactPersonMapped where
  convert tuple = (CP.ContactPersonId $ sel1 tuple, C.CompanyId $ sel2 tuple, 
    (uncurryN $ const $ const CP.ContactPerson) tuple)
instance ColumnToRecord (Maybe Int, Maybe Int, Maybe String, Maybe String, Maybe String) MaybeContactPersonMapped where
  convert tuple = let
    maybeCp = pure CP.ContactPerson <*> sel3 tuple <*> sel4 tuple <*> sel5 tuple
    in (CP.ContactPersonId `fmap` sel1 tuple, C.CompanyId `fmap` sel2 tuple, maybeCp)
instance ColumnToRecord (Maybe Int, Maybe String, Maybe String, Maybe String) MaybeEmployeeMapped where
  convert tuple = (E.EmployeeId `fmap` sel1 tuple, pure E.Employee <*> sel2 tuple <*> sel3 tuple <*> sel4 tuple)
instance ColumnToRecord (Int, Day, Bool, Maybe Int, String, String, String) UpkeepMapped where
  convert tuple = let
    (_,a,b,_,c,d,e) = tuple
    in (U.UpkeepId $ sel1 tuple, E.EmployeeId `fmap` sel4 tuple, U.Upkeep (dayToYmd a) b c d e)
instance ColumnToRecord (Int, String, String, String) EmployeeMapped where
  convert tuple = (E.EmployeeId $ sel1 tuple, uncurryN (const E.Employee) $ tuple)
instance ColumnToRecord (Int, String, Int, Int, Bool) UpkeepSequenceMapped where
  convert (a,b,c,d,e) = (MT.MachineTypeId d, US.UpkeepSequence a b c e)
instance ColumnToRecord (Int, String) CompressorMapped where
  convert tuple = (M.MachineId $ sel1 tuple, MC.Compressor $ sel2 tuple)
instance ColumnToRecord (Int, String) DryerMapped where
  convert tuple = (M.MachineId $ sel1 tuple, MD.Dryer $ sel2 tuple)
instance ColumnToRecord (Int, String, Int, Int, Bool) UpkeepMachineMapped where
  convert (a,b,c,d,e) = (U.UpkeepId a, M.MachineId c, UM.UpkeepMachine b d e)
instance ColumnToRecord (Int, String, String) PhotoMetaMapped where
  convert tuple = (P.PhotoId $ sel1 tuple, (uncurryN $ const PM.PhotoMeta) tuple)

instance (ColumnToRecord a b) => ColumnToRecord [a] [b] where
  convert rows = fmap convert rows

-- todo rather do two queries
mapUpkeeps :: [((Int, Day, Bool, Maybe Int, String, String, String), (Int, String, Int, Int, Bool))] 
           -> [(U.UpkeepId, U.Upkeep, Maybe E.EmployeeId, [(UM.UpkeepMachine, M.MachineId)])]
mapUpkeeps rows = foldl (\acc (upkeepCols, upkeepMachineCols) ->
  let
    upkeepToAdd = convert upkeepCols :: UpkeepMapped
    upkeepMachineToAdd' = convert upkeepMachineCols :: UpkeepMachineMapped
    upkeepMachineToAdd = (sel3 upkeepMachineToAdd', sel2 upkeepMachineToAdd')
    addUpkeep' = (sel1 upkeepToAdd, sel3 upkeepToAdd, sel2 upkeepToAdd, [upkeepMachineToAdd])
    in case acc of
      [] -> [addUpkeep']
      row : rest | sel1 row == sel1 upkeepToAdd -> let
        modifiedUpkeepMachines = upkeepMachineToAdd : sel4 row
        in upd4 modifiedUpkeepMachines row : rest
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
machineManufacturersQuery str = distinct $ proc () -> do
  (_,_,_,manufacturer') <- machineTypesQuery -< ()
  restrict -< (lower manufacturer' `like` (lower $ pgString ("%" ++ (intersperse '%' str) ++ "%")))
  returnA -< manufacturer'

type QEither a b = Either (Query a) (Query b)
machineSpecificQuery :: Int -> Int -> QEither CompressorsTable DryersTable
machineSpecificQuery machineKind machineId = if machineKind == 0
  then Left $ proc () -> do
    compressor <- join compressorsQuery -< pgInt4 machineId
    returnA -< compressor
  else if machineKind == 1
  then Right $ proc () -> do
    dryer <- join dryersQuery -< pgInt4 machineId
    returnA -< dryer
  else undefined

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

like :: Column PGText -> Column PGText -> Column PGBool
like = C.binOp HPQ.OpLike

machineTypesQuery' :: String -> Query DBText
machineTypesQuery' mid = proc () -> do
  (_,_,name',_) <- machineTypesQuery -< ()
  restrict -< (lower name' `like` (lower $ pgString ("%" ++ (intersperse '%' mid) ++ "%")))
  returnA -< name'

companyByIdQuery :: Int -> Query (CompaniesTable)
companyByIdQuery companyId = proc () -> do
  company <- join companiesQuery -< pgInt4 companyId
  returnA -< company

machineTypesWithCountQuery :: Query (MachineTypesTable, DBInt8)
machineTypesWithCountQuery = let 
  query' = proc () -> do
    (machinePK,_,_,machineTypeFK,_,_,_,_,_,_) <- machinesQuery -< ()
    mt <- join machineTypesQuery -< (machineTypeFK)
    returnA -< (mt, machinePK)
  aggregatedQuery = AGG.aggregate (p2(p4(AGG.groupBy, AGG.min, AGG.min, AGG.min),p1(AGG.count))) query'
  orderedQuery = orderBy (asc(\((_,_,name',_),_) -> name')) aggregatedQuery
  in orderedQuery

machinesInCompanyQuery :: Int -> Query (MachinesTable, MachineTypesTable, ContactPersonsLeftJoinTable)
machinesInCompanyQuery companyId = let
  machinesQ = orderBy (asc(\(machine,_) -> sel2 machine)) $ proc () -> do
    m @ (_,companyFK,_,machineTypeFK,_,_,_,_,_,_) <- machinesQuery -< ()
    mt <- join machineTypesQuery -< machineTypeFK
    restrict -< (pgInt4 companyId .== companyFK)
    returnA -< (m, mt)
  joined = leftJoin 
    machinesQ 
    contactPersonsQuery 
    (\((machineRow,_), cpRow) -> sel3 machineRow .== (maybeToNullable $ Just $ sel1 cpRow))
  in proc () -> do
    ((a,b),c) <- joined -< ()
    returnA -< (a,b,c)

companyUpkeepsQuery :: Int -> Query (UpkeepsTable, EmployeeLeftJoinTable)
companyUpkeepsQuery companyId = let 
  upkeepsQuery' = proc () -> do
    (upkeepFK,_,machineFK,_,_) <- upkeepMachinesQuery -< ()
    (_,companyFK,_,_,_,_,_,_,_,_) <- join machinesQuery -< machineFK
    upkeep @ (_,_,closed,_,_,_,_) <- join upkeepsQuery -< upkeepFK
    restrict -< (closed .== pgBool True)
    restrict -< (companyFK .== pgInt4 companyId)
    returnA -< upkeep
  aggregatedUpkeepsQuery = AGG.aggregate (p7(AGG.groupBy, AGG.min, AGG.boolOr, AGG.min, 
    AGG.min, AGG.min, AGG.min)) upkeepsQuery'
  joinedEmployeesQuery = leftJoin aggregatedUpkeepsQuery employeesQuery (
    (\((_,_,_,maybeEmployeeFK,_,_,_),(employeePK,_,_,_)) -> 
      maybeEmployeeFK .== (maybeToNullable $ Just employeePK)))
  orderedUpkeepQuery = orderBy (asc(\((_,date,_,_,_,_,_),_) -> date)) $ joinedEmployeesQuery
  in orderedUpkeepQuery

-- | query, that returns expanded machine type, not just the id
expandedMachinesQuery :: Maybe Int -> Query (MachinesTable, MachineTypesTable)
expandedMachinesQuery machineId = proc () -> do
  machineRow @ (machineId',_,_,machineTypeId,_,_,_,_,_,_) <- machinesQuery -< ()
  machineTypesRow <- join machineTypesQuery -< (machineTypeId)
  restrict -< (case machineId of
    Just(machineId'') -> (pgInt4 machineId'' .== machineId')
    Nothing -> pgBool True )
  returnA -< (machineRow, machineTypesRow)

machineDetailQuery :: Int -> Query (MachinesTable, MachineTypesTable, ContactPersonsLeftJoinTable)
machineDetailQuery machineId = let
  joined = leftJoin 
    (expandedMachinesQuery $ Just machineId)
    contactPersonsQuery 
    (\((machineRow,_), cpRow) -> sel3 machineRow .== (maybeToNullable $ Just $ sel1 cpRow))
  in proc () -> do
    ((m,mt), cp) <- joined -< ()
    returnA -< (m, mt, cp)

machinesInCompanyByUpkeepQuery :: Int -> Query (DBInt, MachinesTable, MachineTypesTable)
machinesInCompanyByUpkeepQuery upkeepId = let
  companyPKQuery = limit 1 $ proc () -> do
    (_,_,machineFK,_,_) <- join upkeepMachinesQuery -< pgInt4 upkeepId
    (_,companyFK,_,_,_,_,_,_,_,_) <- join machinesQuery -< machineFK
    returnA -< companyFK
  in proc () -> do
    companyPK <- companyPKQuery -< ()
    m @ (_,companyFK,_,machineTypeFK,_,_,_,_,_,_) <- machinesQuery -< ()
    restrict -< (companyFK .== companyPK)
    mt <- join machineTypesQuery -< machineTypeFK
    returnA -< (companyPK, m, mt)

expandedUpkeepsQuery2 :: Int -> Query (UpkeepsTable, UpkeepMachinesTable)
expandedUpkeepsQuery2 upkeepId = proc () -> do
  upkeepRow <- join upkeepsQuery -< pgInt4 upkeepId
  upkeepMachineRow <- join upkeepMachinesQuery -< pgInt4 upkeepId
  returnA -< (upkeepRow, upkeepMachineRow)

expandedUpkeepsQuery :: Query (UpkeepsTable, UpkeepMachinesTable)
expandedUpkeepsQuery = proc () -> do
  upkeepRow @ (upkeepPK,_,_,_,_,_,_) <- upkeepsQuery -< ()
  upkeepMachineRow <- join upkeepMachinesQuery -< upkeepPK
  returnA -< (upkeepRow, upkeepMachineRow)

contactPersonsByIdQuery :: Int -> Query ContactPersonsTable
contactPersonsByIdQuery companyId = proc () -> do
  contactPersonRow <- contactPersonsQuery -< ()
  restrict -< sel2 contactPersonRow .== pgInt4 companyId
  returnA -< contactPersonRow

nextServiceMachinesQuery :: Int -> Query MachinesTable
nextServiceMachinesQuery companyId = proc () -> do
  machineRow <- machinesQuery -< ()
  restrict -< sel2 machineRow .== pgInt4 companyId
  returnA -< machineRow

nextServiceUpkeepsQuery :: Int -> Query UpkeepsTable
nextServiceUpkeepsQuery machineId = proc () -> do
  machineRow <- join machinesQuery -< pgInt4 machineId
  upkeepMachineRow <- upkeepMachinesQuery -< ()
  restrict -< sel3 upkeepMachineRow .== sel1 machineRow
  upkeepRow <- join upkeepsQuery -< sel1 upkeepMachineRow
  returnA -< upkeepRow

upkeepsDataForMachine :: Int -> Query ((UpkeepsTable, UpkeepMachinesTable), EmployeeLeftJoinTable)
upkeepsDataForMachine machineId = let 
  upkeepUpkeepMachine = proc () -> do
    machineRow <- join machinesQuery -< pgInt4 machineId
    upkeepMachineRow <- upkeepMachinesQuery -< ()
    restrict -< sel3 upkeepMachineRow .== sel1 machineRow
    upkeepRow <- join upkeepsQuery -< sel1 upkeepMachineRow
    returnA -< (upkeepRow, upkeepMachineRow)
  employeeJoinedRow = leftJoin upkeepUpkeepMachine employeesQuery (\(upkeepPart,(employeePK,_,_,_)) ->
    ((sel4 $ sel1 upkeepPart) .== (maybeToNullable $ Just employeePK)))
  in employeeJoinedRow

nextServiceUpkeepSequencesQuery :: Int -> Query (DBInt, DBText, DBInt, DBInt, DBBool)
nextServiceUpkeepSequencesQuery machineId = proc () -> do
  machineRow <- join machinesQuery -< pgInt4 machineId
  machineTypeRow <- machineTypesQuery -< ()
  restrict -< sel1 machineTypeRow .== sel4 machineRow
  upkeepSequenceRow <- upkeepSequencesByIdQuery' -< sel1 machineTypeRow
  returnA -< upkeepSequenceRow

expandedUpkeepsByCompanyQuery :: Int -> Query 
  (UpkeepsTable, UpkeepMachinesTable, MachineTypesTable, EmployeeLeftJoinTable)
expandedUpkeepsByCompanyQuery companyId = let
  upkeepsWithMachines = proc () -> do
    upkeepRow @ (upkeepPK,_,_,_,_,_,_) <- upkeepsQuery -< ()
    upkeepMachineRow @ (_,_,machineFK,_,_) <- join upkeepMachinesQuery -< upkeepPK
    machine <- join machinesQuery -< machineFK
    machineType <- join machineTypesQuery -< (sel4 machine)
    restrict -< sel2 machine .== pgInt4 companyId
    returnA -< (upkeepRow, upkeepMachineRow, machineType)
  joinedEmployeesQuery = leftJoin upkeepsWithMachines employeesQuery (
    (\(upkeepTuple,(employeePK,_,_,_)) ->
      (sel4 $ sel1 upkeepTuple) .== (maybeToNullable $ Just employeePK)))
  nestedQuery = orderBy (asc(sel2 . sel1 . sel1)) joinedEmployeesQuery
  flattenedQuery = proc () -> do
    ((a,b,c),e) <- nestedQuery -< ()
    returnA -< (a,b,c,e)
  in flattenedQuery

singleEmployeeQuery :: Int -> Query (EmployeeTable)
singleEmployeeQuery employeeId = proc () -> do
  employeeRow <- join employeesQuery -< (pgInt4 employeeId)
  returnA -< employeeRow

groupedPlannedUpkeepsQuery :: Query (UpkeepsTable, CompaniesTable)
groupedPlannedUpkeepsQuery = let
  plannedUpkeepsQuery = proc () -> do
    upkeepRow @ (upkeepPK,_,upkeepClosed,_,_,_,_) <- upkeepsQuery -< ()
    restrict -< upkeepClosed .== pgBool False
    (_,_,machineFK,_,_) <- join upkeepMachinesQuery -< upkeepPK
    (_,companyFK,_,_,_,_,_,_,_,_) <- join machinesQuery -< machineFK
    companyRow <- join companiesQuery -< companyFK
    returnA -< (upkeepRow, companyRow)
  in orderBy (asc(\((_,date,_,_,_,_,_), _) -> date)) $ 
    AGG.aggregate (p2 (p7(AGG.groupBy, AGG.min, AGG.boolOr, AGG.min, AGG.min, AGG.min, AGG.min),
      p6(AGG.min, AGG.min, AGG.min, AGG.min, AGG.min, AGG.min))) plannedUpkeepsQuery

singleContactPersonQuery :: Int -> Query ContactPersonsTable
singleContactPersonQuery contactPersonId = proc () -> do
  contactPersonRow <- join contactPersonsQuery -< pgInt4 contactPersonId
  returnA -< contactPersonRow

singleMachineTypeQuery :: Either String Int -> Query MachineTypesTable
singleMachineTypeQuery machineTypeSid = proc () -> do
  machineTypeNameRow @ (mtId',_,name',_) <- machineTypesQuery -< ()
  restrict -< case machineTypeSid of
    Right(machineTypeId) -> (mtId' .== pgInt4 machineTypeId)
    Left(machineTypeName) -> (name' .== pgString machineTypeName)
  returnA -< machineTypeNameRow

machinesInUpkeepQuery :: Int -> Query UpkeepMachinesTable
machinesInUpkeepQuery upkeepId = proc () -> do
  upkeepMachine <- join upkeepMachinesQuery -< pgInt4 upkeepId
  returnA -< (upkeepMachine)

runMachinesInCompanyQuery :: Int -> Connection -> 
  IO[(M.MachineId, M.Machine, C.CompanyId, MT.MachineTypeId, MT.MachineType, Maybe CP.ContactPerson)]
runMachinesInCompanyQuery companyId connection = do
  rows <- (runQuery connection (machinesInCompanyQuery companyId))
  let 
    mapRow row = let
      (machine :: MachineMapped, machineType :: MachineTypeMapped, 
        contactPerson :: MaybeContactPersonMapped) = convertDeep row
      in (sel1 machine, sel5 machine, sel2 machine, 
        sel1 machineType, sel2 machineType, sel3 contactPerson)
  return $ fmap mapRow rows

runExpandedMachinesQuery' :: Maybe Int -> Connection 
  -> IO[((Int, Int, Maybe Int, Int, Maybe Day, Int, Int, String, String, String), (Int, Int, String, String))]
runExpandedMachinesQuery' machineId connection =
  runQuery connection (expandedMachinesQuery machineId)

runCompanyUpkeepsQuery :: Int -> Connection -> 
  IO[((Int, Day, Bool, Maybe Int, String, String, String), (Maybe Int, Maybe String, Maybe String, Maybe String))]
runCompanyUpkeepsQuery companyId connection = 
  runQuery connection (companyUpkeepsQuery companyId)

convertExpanded row = let 
  (m :: MachineMapped, mt :: MachineTypeMapped) = convertDeep row
  in (sel1 m, sel5 m, sel2 m, sel1 mt, sel2 mt)

runExpandedMachinesQuery :: Maybe Int -> Connection -> IO[(M.MachineId, M.Machine, C.CompanyId, MT.MachineTypeId, MT.MachineType)]
runExpandedMachinesQuery machineId connection = do
  rows <- runExpandedMachinesQuery' machineId connection
  return $ fmap convertExpanded rows

runMachineTypesQuery' :: String -> Connection -> IO[String]
runMachineTypesQuery' mid connection = runQuery connection (machineTypesQuery' mid)

runMachinesInCompanyByUpkeepQuery :: Int -> Connection -> 
  IO[(C.CompanyId, (M.MachineId, M.Machine, C.CompanyId, MT.MachineTypeId, MT.MachineType))]
runMachinesInCompanyByUpkeepQuery upkeepId connection = do
  rows <- runQuery connection (machinesInCompanyByUpkeepQuery upkeepId)
  return $ map (\(companyId,a,b) -> (C.CompanyId companyId, convertExpanded (a,b))) rows

withConnection :: (Connection -> IO a) -> IO a
withConnection runQ = do
  let connectInfo = defaultConnectInfo {
    connectUser = "haskell" ,
    connectDatabase = "crm" ,
    connectPassword = "haskell" ,
    connectHost = "localhost" }
  conn <- connect connectInfo
  result <- runQ conn
  close conn
  return result

getMachinePhoto :: Connection
                -> Int
                -> IO ByteString
getMachinePhoto connection photoId = do
  let q = " select data from photos where id = ? "
  result <- query connection q (Only photoId)
  return $ fromOnly $ head $ result

addMachinePhoto :: Connection
                -> Int
                -> ByteString
                -> IO [Int]
addMachinePhoto connection _ photo = do
  let q = " insert into photos(data) values (?) returning id "
  newIds <- query connection q (Only $ Binary photo)
  let ints = map (\(Only id') -> id') newIds
  return ints

addCompany :: Connection -- ^ database connection
           -> C.Company -- ^ company to save in the db
           -> IO Int
addCompany connection newCompany = do
  newId <- runInsertReturning
    connection
    companiesTable (Nothing, pgString $ C.companyName newCompany, pgString $ C.companyPlant newCompany, 
      pgString $ C.companyAddress newCompany, maybeToNullable $ Nothing, maybeToNullable $ Nothing)
    sel1
  return $ head newId -- todo safe

singleRowOrColumn :: Monad m
                  => [a] 
                  -> ExceptT (Reason r) m a
singleRowOrColumn result = case result of
  row : xs | null xs -> return row
  [] -> throwError $ InputError $ ParseError "no record"
  _ -> throwError $ InputError $ ParseError "more than one record failure"
