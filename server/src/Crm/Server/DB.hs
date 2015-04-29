{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

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
  photosTable ,
  contactPersonsTable ,
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
  -- manipulations
  addCompany ,
  addMachinePhoto ,
  runMachineUpdate ,
  -- runs
  runExpandedMachinesQuery ,
  runCompaniesQuery ,
  runMachinesInCompanyQuery ,
  runCompanyWithMachinesQuery ,
  runMachineTypesQuery' ,
  runExpandedUpkeepsQuery ,
  runPlannedUpkeepsQuery ,
  runSingleUpkeepQuery ,
  runMachinesInCompanyByUpkeepQuery ,
  runCompanyUpkeepsQuery ,
  -- more complex query
  nextServiceMachinesQuery ,
  nextServiceUpkeepsQuery ,
  nextServiceUpkeepSequencesQuery ,
  upkeepsDataForMachine ,
  expandedUpkeepsByCompanyQuery ,
  machineTypesWithCountQuery ,
  upkeepSequencesByIdQuery ,
  singleMachineTypeQuery ,
  machinesInUpkeepQuery ,
  machinePhotosByMachineId ,
  photoMetaQuery ,
  machineManufacturersQuery ,
  -- helpers
  withConnection ,
  singleRowOrColumn ) where

import Database.PostgreSQL.Simple (ConnectInfo(..), Connection, defaultConnectInfo, connect, close, query,
  Only(..), Binary(..), returning, query_ )

import Opaleye.QueryArr (Query, QueryArr)
import Opaleye.Table (Table(Table), required, queryTable, optional)
import Opaleye.Column (Column, Nullable)
import Opaleye.Order (orderBy, asc, limit, desc)
import Opaleye.RunQuery (runQuery)
import Opaleye.Operators ((.==), (.&&), (.||), restrict, lower, (.<))
import qualified Opaleye.Operators as OO
import Opaleye.PGTypes (pgInt4, PGDate, pgDay, PGBool, PGInt4, PGInt8, PGText, pgString, pgBool)
import Opaleye.Manipulation (runUpdate, runInsertReturning, runInsert)
import qualified Opaleye.Aggregate as AGG
import Opaleye.Join (leftJoin)
import Opaleye.Distinct (distinct)

import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Error (ErrorT)
import Control.Monad (liftM, forM)
import Control.Arrow (returnA)

import Data.Profunctor.Product (p1, p2, p3, p4, p5, p6, p7, p9)
import Data.Time.Calendar (Day, addDays)
import Data.Int (Int64)
import Data.List (intersperse)
import Data.Tuple.All (Sel1, sel1, sel2, sel3, sel4)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 (unpack)

import Rest.Types.Error (DataError(ParseError), Reason(InputError))

import qualified Crm.Shared.Company as C
import qualified Crm.Shared.Machine as M
import qualified Crm.Shared.MachineType as MT
import qualified Crm.Shared.YearMonthDay as D

import Crm.Server.Helpers (ymdToDay, dayToYmd, maybeToNullable)

import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ
import Opaleye.PGTypes (literalColumn)
import Opaleye.Column (Column)
import qualified Opaleye.Internal.Column as C

type DBInt = Column PGInt4
type DBInt8 = Column PGInt8
type DBText = Column PGText
type DBDate = Column PGDate
type DBBool = Column PGBool
type DBByteA = Column PGByteA

type CompaniesTable = (DBInt, DBText, DBText, DBText)
type CompaniesWriteTable = (Maybe DBInt, DBText, DBText, DBText)

type ContactPersonTable = (DBInt, DBText, DBText, DBText)
type ContactPersonWriteTable = (Maybe DBInt, DBText, DBText, DBText)

type MachinesTable = (DBInt, DBInt, DBInt, Column (Nullable PGDate), DBInt, DBInt, DBText, DBText, DBText)
type MachinesWriteTable = (Maybe DBInt, DBInt, DBInt, Column (Nullable PGDate), DBInt, DBInt, DBText, DBText, DBText)

type MachineTypesTable = (DBInt, DBText, DBText)
type MachineTypesWriteTable = (Maybe DBInt, DBText, DBText)

type UpkeepsTable = (DBInt, DBDate, DBBool, Column (Nullable PGInt4), DBText, DBText, DBText)
type UpkeepsWriteTable = (Maybe DBInt, DBDate, DBBool, Column (Nullable PGInt4), DBText, DBText, DBText)

type UpkeepMachinesTable = (DBInt, DBText, DBInt, DBInt, DBBool)

type EmployeeTable = (DBInt, DBText, DBText, DBText)
type EmployeeLeftJoinTable = (Column (Nullable PGInt4), Column (Nullable PGText), Column (Nullable PGText), Column (Nullable PGText))
type EmployeeWriteTable = (Maybe DBInt, DBText, DBText, DBText)

type UpkeepSequencesTable = (DBInt, DBText, DBInt, DBInt, DBBool)

type PhotosMetaTable = (DBInt, DBText, DBText)

type PhotosTable = (DBInt, DBByteA)

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

photosTable :: Table PhotosTable PhotosTable
photosTable = Table "photos" $ p2 (
  required "id" ,
  required "data" )

companiesTable :: Table CompaniesWriteTable CompaniesTable
companiesTable = Table "companies" $ p4 (
  optional "id" ,
  required "name" ,
  required "plant" ,
  required "address" )

contactPersonsTable :: Table ContactPersonWriteTable ContactPersonTable
contactPersonsTable = Table "contact_persons" $ p4 (
  optional "id" ,
  required "name" ,
  required "phone" ,
  required "position" )

machinesTable :: Table MachinesWriteTable MachinesTable
machinesTable = Table "machines" $ p9 (
  optional "id" ,
  required "company_id" ,
  required "machine_type_id" ,
  required "operation_start" ,
  required "initial_mileage" ,
  required "mileage_per_year" ,
  required "note" ,
  required "serial_number" ,
  required "year_of_manufacture" )

machineTypesTable :: Table MachineTypesWriteTable MachineTypesTable
machineTypesTable = Table "machine_types" $ p3 (
  optional "id" ,
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

contactPersonsQuery :: Query ContactPerson
contactPersonsQuery = queryTable contactPersonsTable

photosQuery :: Query PhotosTable
photosQuery = queryTable photosTable

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
  (_, _, manufacturer') <- machineTypesQuery -< ()
  restrict -< (lower manufacturer' `like` (lower $ pgString ("%" ++ (intersperse '%' str) ++ "%")))
  returnA -< manufacturer'

photoMetaQuery :: Int -> Query PhotosMetaTable
photoMetaQuery photoId = proc () -> do
  result <- join photosMetaQuery -< pgInt4 photoId
  returnA -< result

upkeepSequencesByIdQuery' :: QueryArr DBInt (DBInt, DBText, DBInt, DBBool)
upkeepSequencesByIdQuery' = proc (machineTypeId) -> do
  (a,b,c,machineTypeFK,d) <- upkeepSequencesQuery -< ()
  restrict -< machineTypeFK .== machineTypeId
  returnA -< (a,b,c,d)

upkeepSequencesByIdQuery :: DBInt -> Query (DBInt, DBText, DBInt, DBBool)
upkeepSequencesByIdQuery machineTypeId = proc () -> do
  upkeepSequenceRow' <- upkeepSequencesByIdQuery' -< machineTypeId
  returnA -< upkeepSequenceRow'

actualUpkeepRepetitionQuery :: Int -> Query DBInt
actualUpkeepRepetitionQuery machineTypeId' = let  
  machineTypeId = pgInt4 machineTypeId'
  -- find out maximum recorded mileages
  -- to find out later, if the mileage are enough so the 
  -- initial upkeep should already passed
  mileages = proc () -> do
    (_,_,machineTypeFK,_,initialMileage,_,_,_,_) <- machinesQuery -< ()
    restrict -< machineTypeFK .== machineTypeId
    (_,_,_,recordedMileage,_) <- upkeepMachinesQuery -< ()
    returnA -< (recordedMileage, initialMileage)
  maxMileages = AGG.aggregate (p2 (AGG.max, AGG.max)) mileages

  upkeepsInOneMachineType = proc () -> do
    (recordedMileage, initialMileage) <- maxMileages -< ()
    (_,_,repetition,machineTypeFK,oneTime) <- upkeepSequencesQuery -< ()
    restrict -< machineTypeFK .== machineTypeId
    restrict -< (OO.not oneTime) .|| (recordedMileage .< repetition .&& initialMileage .< repetition)
    returnA -< repetition
  
  in AGG.aggregate (p1 AGG.min) upkeepsInOneMachineType

runMachineUpdate :: (Int, M.Machine) 
                 -> Connection 
                 -> IO Int64
runMachineUpdate (machineId', machine') connection =
  runUpdate connection machinesTable readToWrite condition
    where
      condition (machineId,_,_,_,_,_,_,_,_) = machineId .== pgInt4 machineId'
      readToWrite (_,companyId, machineTypeId,_,_,_,_,_,_) =
        (Nothing, companyId, machineTypeId,
          maybeToNullable $ fmap (pgDay . ymdToDay) (M.machineOperationStartDate machine'),
          pgInt4 $ M.initialMileage machine', pgInt4 $ M.mileagePerYear machine', 
          pgString $ M.note machine', pgString $ M.serialNumber machine',
          pgString $ M.yearOfManufacture machine' )

like :: Column PGText -> Column PGText -> Column PGBool
like = C.binOp HPQ.OpLike

machineTypesQuery' :: String -> Query DBText
machineTypesQuery' mid = proc () -> do
  (_,name',_) <- machineTypesQuery -< ()
  restrict -< (lower name' `like` (lower $ pgString ("%" ++ (intersperse '%' mid) ++ "%")))
  returnA -< name'

companyWithMachinesQuery :: Int -> Query (CompaniesTable)
companyWithMachinesQuery companyId = proc () -> do
  company <- companiesQuery -< ()
  restrict -< (pgInt4 companyId .== sel1 company)
  returnA -< company

runCompanyWithMachinesQuery :: Int -> Connection -> IO[(Int,String,String,String)]
runCompanyWithMachinesQuery companyId connection =
  runQuery connection (companyWithMachinesQuery companyId)

machineTypesWithCountQuery :: Query (MachineTypesTable, DBInt8)
machineTypesWithCountQuery = let 
  query = proc () -> do
    (machinePK,_,machineTypeFK,_,_,_,_,_,_) <- machinesQuery -< ()
    mt <- join machineTypesQuery -< (machineTypeFK)
    returnA -< (mt, machinePK)
  aggregatedQuery = AGG.aggregate (p2(p3(AGG.groupBy, AGG.min, AGG.min),p1(AGG.count))) query
  orderedQuery = orderBy (asc(\((_,name',_),_) -> name')) aggregatedQuery
  in orderedQuery

machinesInCompanyQuery :: Int -> Query (MachinesTable, MachineTypesTable)
machinesInCompanyQuery companyId = orderBy 
  (asc(\((machineId,_,_,_,_,_,_,_,_),_) -> machineId)) $ proc () -> do
    m @ (_,companyFK,machineTypeFK,_,_,_,_,_,_) <- machinesQuery -< ()
    mt <- join machineTypesQuery -< machineTypeFK
    restrict -< (pgInt4 companyId .== companyFK)
    returnA -< (m, mt)

companyUpkeepsQuery :: Int -> Query (UpkeepsTable, EmployeeLeftJoinTable)
companyUpkeepsQuery companyId = let 
  upkeepsQuery' = proc () -> do
    (upkeepFK,_,machineFK,_,_) <- upkeepMachinesQuery -< ()
    (_,companyFK,_,_,_,_,_,_,_) <- join machinesQuery -< machineFK
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
  machineRow @ (machineId',_,machineTypeId,_,_,_,_,_,_) <- machinesQuery -< ()
  machineTypesRow <- join machineTypesQuery -< (machineTypeId)
  restrict -< (case machineId of
    Just(machineId'') -> (pgInt4 machineId'' .== machineId')
    Nothing -> pgBool True )
  returnA -< (machineRow, machineTypesRow)

machinesInCompanyByUpkeepQuery :: Int -> Query (DBInt, MachinesTable, MachineTypesTable)
machinesInCompanyByUpkeepQuery upkeepId = let
  companyPKQuery = limit 1 $ proc () -> do
    (_,_,machineFK,_,_) <- join upkeepMachinesQuery -< pgInt4 upkeepId
    (_,companyFK,_,_,_,_,_,_,_) <- join machinesQuery -< machineFK
    returnA -< companyFK
  in proc () -> do
    companyPK <- companyPKQuery -< ()
    m @ (_,companyFK,machineTypeFK,_,_,_,_,_,_) <- machinesQuery -< ()
    restrict -< (companyFK .== companyPK)
    mt <- join machineTypesQuery -< machineTypeFK
    returnA -< (companyPK, m, mt)

lastClosedMaintenanceQuery :: Int -> Query (UpkeepsTable, UpkeepMachinesTable)
lastClosedMaintenanceQuery machineId = limit 1 $ orderBy (desc(\((_,date,_,_,_,_,_),_) -> date)) $ 
    proc () -> do
  upkeepMachineRow @ (upkeepFK,_,_,_,_) <- join upkeepMachinesQuery -< pgInt4 machineId
  upkeepRow @ (_,_,upkeepClosed,_,_,_,_) <- join upkeepsQuery -< upkeepFK
  restrict -< pgBool True .== upkeepClosed
  returnA -< (upkeepRow, upkeepMachineRow)

nextMaintenanceQuery :: Int -> Query (UpkeepsTable)
nextMaintenanceQuery machineId = limit 1 $ orderBy (asc(\(_,date,_,_,_,_,_) -> date)) $ proc () -> do
  upkeepRow @ (upkeepPK,_,upkeepClosed,_,_,_,_) <- upkeepsQuery -< ()
  restrict -< (upkeepClosed .== pgBool False)
  (_,_,machineFK,_,_) <- join upkeepMachinesQuery -< upkeepPK
  restrict -< pgInt4 machineId .== machineFK
  returnA -< upkeepRow

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

nextServiceUpkeepSequencesQuery :: Int -> Query (DBInt, DBText, DBInt, DBBool)
nextServiceUpkeepSequencesQuery machineId = proc () -> do
  machineRow <- join machinesQuery -< pgInt4 machineId
  machineTypeRow <- machineTypesQuery -< ()
  restrict -< sel1 machineTypeRow .== sel3 machineRow
  upkeepSequenceRow <- upkeepSequencesByIdQuery' -< sel1 machineTypeRow
  returnA -< upkeepSequenceRow

expandedUpkeepsByCompanyQuery :: Int -> Query 
  (UpkeepsTable, UpkeepMachinesTable, MachineTypesTable, DBInt, EmployeeLeftJoinTable)
expandedUpkeepsByCompanyQuery companyId = let
  upkeepsWithMachines = proc () -> do
    upkeepRow @ (upkeepPK,_,_,_,_,_,_) <- upkeepsQuery -< ()
    upkeepMachineRow @ (_,_,machineFK,_,_) <- join upkeepMachinesQuery -< upkeepPK
    machine <- join machinesQuery -< machineFK
    machineType <- join machineTypesQuery -< (sel3 machine)
    restrict -< sel2 machine .== pgInt4 companyId
    returnA -< (upkeepRow, upkeepMachineRow, machineType, sel1 machine)
  joinedEmployeesQuery = leftJoin upkeepsWithMachines employeesQuery (
    (\(upkeepTuple,(employeePK,_,_,_)) ->
      (sel4 $ sel1 upkeepTuple) .== (maybeToNullable $ Just employeePK)))
  nestedQuery = orderBy (asc(sel2 . sel1 . sel1)) joinedEmployeesQuery
  flattenedQuery = proc () -> do
    ((a,b,c,d),e) <- nestedQuery -< ()
    returnA -< (a,b,c,d,e)
  in flattenedQuery

singleEmployeeQuery :: Int -> Query (EmployeeTable)
singleEmployeeQuery employeeId = proc () -> do
  employeeRow <- join employeesQuery -< (pgInt4 employeeId)
  returnA -< employeeRow

plannedUpkeepsQuery :: Query (UpkeepsTable, CompaniesTable)
plannedUpkeepsQuery = proc () -> do
  upkeepRow @ (upkeepPK,_,upkeepClosed,_,_,_,_) <- upkeepsQuery -< ()
  restrict -< upkeepClosed .== pgBool False
  (_,_,machineFK,_,_) <- join upkeepMachinesQuery -< upkeepPK
  (_,companyFK,_,_,_,_,_,_,_) <- join machinesQuery -< machineFK
  companyRow <- join companiesQuery -< companyFK
  returnA -< (upkeepRow, companyRow)

groupedPlannedUpkeepsQuery :: Query (UpkeepsTable, CompaniesTable)
groupedPlannedUpkeepsQuery = orderBy (asc(\((_,date,_,_,_,_,_), _) -> date)) $ 
  AGG.aggregate (p2 (p7(AGG.groupBy, AGG.min, AGG.boolOr, AGG.min, AGG.min, AGG.min, AGG.min),
    p4(AGG.min, AGG.min, AGG.min, AGG.min))) (plannedUpkeepsQuery)

singleMachineTypeQuery :: Either String Int -> Query MachineTypesTable
singleMachineTypeQuery machineTypeSid = proc () -> do
  machineTypeNameRow @ (mtId',name',_) <- machineTypesQuery -< ()
  restrict -< case machineTypeSid of
    Right(machineTypeId) -> (mtId' .== pgInt4 machineTypeId)
    Left(machineTypeName) -> (name' .== pgString machineTypeName)
  returnA -< machineTypeNameRow

machinesInUpkeepQuery :: Int -> Query UpkeepMachinesTable
machinesInUpkeepQuery upkeepId = proc () -> do
  upkeepMachine <- join upkeepMachinesQuery -< pgInt4 upkeepId
  returnA -< (upkeepMachine)

runCompaniesQuery :: Connection -> IO [(Int, String, String, String)]
runCompaniesQuery connection = runQuery connection companiesQuery

runMachinesInCompanyQuery' :: Int -> Connection ->
  IO[((Int, Int, Int, Maybe Day, Int, Int, String, String, String), (Int, String, String))]
runMachinesInCompanyQuery' companyId connection =
  runQuery connection (machinesInCompanyQuery companyId)

runMachinesInCompanyQuery :: Int -> Connection -> IO[(Int, M.Machine, Int, Int, MT.MachineType)]
runMachinesInCompanyQuery companyId connection = do
  rows <- (runMachinesInCompanyQuery' companyId connection)
  return $ map convertExpanded rows

convertExpanded :: ((Int, Int, Int, Maybe Day, Int, Int, String, String, String),(Int, String, String)) 
                -> (Int, M.Machine, Int, Int, MT.MachineType)
convertExpanded = (\((mId,cId,_,mOs,m3,m4,m5,m6,m7),(mtId,mtN,mtMf)) ->
  (mId, M.Machine (fmap dayToYmd mOs) m3 m4 m5 m6 m7, cId, mtId, (MT.MachineType mtN mtMf)))

runExpandedMachinesQuery' :: Maybe Int -> Connection 
  -> IO[((Int, Int, Int, Maybe Day, Int, Int, String, String, String), (Int, String, String))]
runExpandedMachinesQuery' machineId connection =
  runQuery connection (expandedMachinesQuery machineId)

runCompanyUpkeepsQuery :: Int -> Connection -> 
  IO[((Int, Day, Bool, Maybe Int, String, String, String), (Maybe Int, Maybe String, Maybe String, Maybe String))]
runCompanyUpkeepsQuery companyId connection = 
  runQuery connection (companyUpkeepsQuery companyId)

runExpandedMachinesQuery :: Maybe Int -> Connection -> IO[(Int, M.Machine, Int, Int, MT.MachineType)]
runExpandedMachinesQuery machineId connection = do
  rows <- runExpandedMachinesQuery' machineId connection
  return $ fmap convertExpanded rows

runMachineTypesQuery' :: String -> Connection -> IO[String]
runMachineTypesQuery' mid connection = runQuery connection (machineTypesQuery' mid)

runLastClosedMaintenanceQuery :: Int -> Connection -> IO[
  ((Int, Day, Bool, Maybe Int, String, String, String),(Int, String, Int, Int, Bool))]
runLastClosedMaintenanceQuery machineId connection =
  runQuery connection (lastClosedMaintenanceQuery machineId)

runNextMaintenanceQuery :: Int -> Connection -> 
  IO[(Int, Day, Bool, Maybe Int, String, String, String)]
runNextMaintenanceQuery machineId connection = runQuery connection (nextMaintenanceQuery machineId)

runExpandedUpkeepsQuery :: Connection -> IO[((Int, Day, Bool, Maybe Int, String, String, String),
  (Int, String, Int, Int, Bool))]
runExpandedUpkeepsQuery connection = runQuery connection expandedUpkeepsQuery

runMachinesInCompanyByUpkeepQuery :: Int -> Connection -> IO[(Int, (Int, M.Machine, Int, Int, MT.MachineType))]
runMachinesInCompanyByUpkeepQuery upkeepId connection = do
  rows <- runQuery connection (machinesInCompanyByUpkeepQuery upkeepId)
  return $ map (\(companyId,a,b) -> (companyId, convertExpanded (a,b))) rows

runPlannedUpkeepsQuery :: Connection -> IO[((Int, Day, Bool, Maybe Int, String, String, String), 
  (Int, String, String, String))]
runPlannedUpkeepsQuery connection = runQuery connection groupedPlannedUpkeepsQuery

runSingleUpkeepQuery :: Connection 
                     -> Int -- ^ upkeep id
                     -> IO[((Int, Day, Bool, Maybe Int, String, String, String), 
                          (Int, String, Int, Int, Bool))]
runSingleUpkeepQuery connection upkeepId = do
  runQuery connection (expandedUpkeepsQuery2 upkeepId)

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
addMachinePhoto connection machineId photo = do
  let q = " insert into photos(data) values (?) returning id "
  newIds <- query connection q (Only $ Binary photo)
  let ints = map (\(Only id) -> id) newIds
  return ints
  
data PGByteA

pgByteA :: ByteString -> Column PGByteA
pgByteA = literalColumn . HPQ.OtherLit . unpack

addCompany :: Connection -- ^ database connection
           -> C.Company -- ^ company to save in the db
           -> IO Int
addCompany connection newCompany = do
  newId <- runInsertReturning
    connection
    companiesTable (Nothing, pgString $ C.companyName newCompany, pgString $ C.companyPlant newCompany, 
      pgString $ C.companyAddress newCompany)
    sel1
  return $ head newId -- todo safe

singleRowOrColumn :: Monad m
                  => [a] 
                  -> ErrorT (Reason r) m a
singleRowOrColumn result = case result of
  row : xs | null xs -> return row
  _ -> throwError $ InputError $ ParseError "more than one record failure"
