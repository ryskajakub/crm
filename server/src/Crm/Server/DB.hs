{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}

module Crm.Server.DB (
  -- tables
  companiesTable ,
  machinesTable ,
  machineTypesTable ,
  upkeepTable ,
  upkeepMachinesTable ,
  employeesTable ,
  upkeepSequencesTable ,
  -- basic queries
  companiesQuery ,
  machinesQuery ,
  machineTypesQuery ,
  upkeepsQuery ,
  upkeepMachinesQuery ,
  employeesQuery ,
  upkeepSequencesQuery ,
  -- manipulations
  addCompany ,
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
  machineTypesWithCountQuery ,
  upkeepSequencesByIdQuery ,
  singleMachineTypeQuery ,
  -- core computation
  nextService ,
  -- helpers
  withConnection ,
  singleRowOrColumn ) where

import Database.PostgreSQL.Simple (ConnectInfo(..), Connection, defaultConnectInfo, connect, close)

import Opaleye.QueryArr (Query, QueryArr)
import Opaleye.Table (Table(Table), required, queryTable, optional)
import Opaleye.Column (Column, Nullable)
import Opaleye.Order (orderBy, asc, limit, desc)
import Opaleye.RunQuery (runQuery)
import Opaleye.Operators ((.==), (.&&), (.||), restrict, lower, (.<))
import qualified Opaleye.Operators as OO
import Opaleye.PGTypes (pgInt4, PGDate, pgDay, PGBool, PGInt4, PGInt8, PGText, pgString, pgBool)
import Opaleye.Manipulation (runUpdate, runInsertReturning)
import qualified Opaleye.Aggregate as AGG
import Opaleye.Join (leftJoin)

import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Error (ErrorT)
import Control.Arrow (returnA)

import Data.Profunctor.Product (p1, p2, p3, p4, p5, p6, p7)
import Data.Time.Calendar (Day, addDays)
import Data.Int (Int64)
import Data.List (intersperse)
import Data.Tuple.All (Sel1, sel1, sel2)

import Rest.Types.Error (DataError(ParseError), Reason(InputError))

import qualified Crm.Shared.Company as C
import qualified Crm.Shared.Machine as M
import qualified Crm.Shared.MachineType as MT
import qualified Crm.Shared.YearMonthDay as D

import Crm.Server.Helpers (ymdToDay, dayToYmd, maybeToNullable)

import qualified Opaleye.Internal.Column as C
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

type DBInt = Column PGInt4
type DBInt8 = Column PGInt8
type DBText = Column PGText
type DBDate = Column PGDate
type DBBool = Column PGBool

type CompaniesTable = (DBInt, DBText, DBText, DBText, DBText, DBText)
type CompaniesWriteTable = (Maybe DBInt, DBText, DBText, DBText, DBText, DBText)

type MachinesTable = (DBInt, DBInt, DBInt, DBDate, DBInt, DBInt, DBText)
type MachinesWriteTable = (Maybe DBInt, DBInt, DBInt, DBDate, DBInt, DBInt, DBText)

type MachineTypesTable = (DBInt, DBText, DBText)
type MachineTypesWriteTable = (Maybe DBInt, DBText, DBText)

type UpkeepTable = (DBInt, DBDate, DBBool, Column (Nullable PGInt4))
type UpkeepWriteTable = (Maybe DBInt, DBDate, DBBool, (Column (Nullable PGInt4)))

type UpkeepMachinesTable = (DBInt, DBText, DBInt, DBInt)

type EmployeeTable = (DBInt, DBText)
type EmployeeLeftJoinTable = (Column (Nullable PGInt4), Column (Nullable PGText))
type EmployeeWriteTable = (Maybe DBInt, DBText)

type UpkeepSequencesTable = (DBInt, DBText, DBInt, DBInt, DBBool)

companiesTable :: Table CompaniesWriteTable CompaniesTable
companiesTable = Table "companies" (p6 (
  optional "id" ,
  required "name" ,
  required "plant" ,
  required "address" ,
  required "person" ,
  required "phone" ))

machinesTable :: Table MachinesWriteTable MachinesTable
machinesTable = Table "machines" (p7 (
  optional "id" ,
  required "company_id" ,
  required "machine_type_id" ,
  required "operation_start" ,
  required "initial_mileage" ,
  required "mileage_per_year" ,
  required "note" ))

machineTypesTable :: Table MachineTypesWriteTable MachineTypesTable
machineTypesTable = Table "machine_types" $ p3 (
  optional "id" ,
  required "name" ,
  required "manufacturer" )

upkeepTable :: Table UpkeepWriteTable UpkeepTable
upkeepTable = Table "upkeeps" $ p4 (
  optional "id" ,
  required "date_" ,
  required "closed" ,
  required "employee_id" )

upkeepMachinesTable :: Table UpkeepMachinesTable UpkeepMachinesTable
upkeepMachinesTable = Table "upkeep_machines" $ p4 (
  required "upkeep_id" ,
  required "note" ,
  required "machine_id" ,
  required "recorded_mileage" )

employeesTable :: Table EmployeeWriteTable EmployeeTable
employeesTable = Table "employees" $ p2 (
  optional "id" ,
  required "name" )

upkeepSequencesTable :: Table UpkeepSequencesTable UpkeepSequencesTable
upkeepSequencesTable = Table "upkeep_sequences" $ p5 (
  required "display_ordering" ,
  required "label" ,
  required "repetition" ,
  required "machine_type_id" ,
  required "one_time" )

companiesQuery :: Query CompaniesTable
companiesQuery = queryTable companiesTable

machinesQuery :: Query MachinesTable
machinesQuery = queryTable machinesTable

machineTypesQuery :: Query MachineTypesTable
machineTypesQuery = queryTable machineTypesTable

upkeepsQuery :: Query UpkeepTable
upkeepsQuery = queryTable upkeepTable

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

upkeepSequencesByIdQuery :: Int -> Query (DBInt, DBText, DBInt, DBBool)
upkeepSequencesByIdQuery machineTypeId = proc () -> do
  (a,b,c,machineTypeFK, d) <- upkeepSequencesQuery -< ()
  restrict -< machineTypeFK .== pgInt4 machineTypeId
  returnA -< (a,b,c,d)

actualUpkeepRepetitionQuery :: Int -> Query DBInt
actualUpkeepRepetitionQuery machineTypeId' = let  
  machineTypeId = pgInt4 machineTypeId'

  -- find out maximum recorded mileages
  -- to find out later, if the mileage are enough so the 
  -- initial upkeep should already passed
  mileages = proc () -> do
    (_,_,machineTypeFK,_,initialMileage,_,_) <- machinesQuery -< ()
    restrict -< machineTypeFK .== machineTypeId
    (_,_,_,recordedMileage) <- upkeepMachinesQuery -< ()
    returnA -< (recordedMileage, initialMileage)
  maxMileages = AGG.aggregate (p2 (AGG.max, AGG.max)) mileages

  upkeepsInOneMachineType = proc () -> do
    (recordedMileage, initialMileage) <- maxMileages -< ()
    (_,_,repetition,machineTypeFK,oneTime) <- upkeepSequencesQuery -< ()
    restrict -< machineTypeFK .== machineTypeId
    restrict -< (OO.not oneTime) .|| (recordedMileage .< repetition .&& initialMileage .< repetition)
    returnA -< repetition
  
  in AGG.aggregate (p1 AGG.min) upkeepsInOneMachineType

runMachineUpdate :: (Int, Int, M.Machine) 
                 -> Connection 
                 -> IO Int64
runMachineUpdate (machineId', machineTypeId, machine') connection =
  runUpdate connection machinesTable readToWrite condition
    where
      condition (machineId,_,_,_,_,_,_) = machineId .== pgInt4 machineId'
      readToWrite (_,companyId,_,_,_,_,_) =
        (Nothing, companyId, pgInt4 machineTypeId,
          pgDay $ ymdToDay $ M.machineOperationStartDate machine',
          pgInt4 $ M.initialMileage machine', pgInt4 $ M.mileagePerYear machine', 
          pgString $ M.note machine' )

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

runCompanyWithMachinesQuery :: Int -> Connection -> IO[(Int,String,String,String,String,String)]
runCompanyWithMachinesQuery companyId connection =
  runQuery connection (companyWithMachinesQuery companyId)

machineTypesWithCountQuery :: Query (MachineTypesTable, DBInt8)
machineTypesWithCountQuery = let 
  query = proc () -> do
    (machinePK,_,machineTypeFK,_,_,_,_) <- machinesQuery -< ()
    mt <- join machineTypesQuery -< (machineTypeFK)
    returnA -< (mt, machinePK)
  aggregatedQuery = AGG.aggregate (p2(p3(AGG.groupBy, AGG.min, AGG.min),p1(AGG.count))) query
  orderedQuery = orderBy (asc(\((_,name',_),_) -> name')) aggregatedQuery
  in orderedQuery

machinesInCompanyQuery :: Int -> Query (MachinesTable, MachineTypesTable)
machinesInCompanyQuery companyId = orderBy (asc(\((machineId,_,_,_,_,_,_),_) -> machineId)) $ proc () -> do
  m @ (_,companyFK,machineTypeFK,_,_,_,_) <- machinesQuery -< ()
  mt <- join machineTypesQuery -< machineTypeFK
  restrict -< (pgInt4 companyId .== companyFK)
  returnA -< (m, mt)

companyUpkeepsQuery :: Int -> Query (UpkeepTable, EmployeeLeftJoinTable)
companyUpkeepsQuery companyId = let 
  upkeepsQuery' = proc () -> do
    (upkeepFK,_,machineFK,_) <- upkeepMachinesQuery -< ()
    (_,companyFK,_,_,_,_,_) <- join machinesQuery -< machineFK
    upkeep @ (_,_,closed,_) <- join upkeepsQuery -< upkeepFK
    restrict -< (closed .== pgBool True)
    restrict -< (companyFK .== pgInt4 companyId)
    returnA -< upkeep
  aggregatedUpkeepsQuery = AGG.aggregate (p4(AGG.groupBy, AGG.min, AGG.boolOr, AGG.min)) upkeepsQuery'
  joinedEmployeesQuery = leftJoin aggregatedUpkeepsQuery employeesQuery (
    ( \((_,_,_,maybeEmployeeFK),(employeePK,_)) -> maybeEmployeeFK .== (maybeToNullable $ Just employeePK) ))
  orderedUpkeepQuery = orderBy (asc(\((_,date,_,_),(_,_)) -> date)) $ joinedEmployeesQuery
  in orderedUpkeepQuery

-- | query, that returns expanded machine type, not just the id
expandedMachinesQuery :: Maybe Int -> Query (MachinesTable, MachineTypesTable)
expandedMachinesQuery machineId = proc () -> do
  machineRow @ (machineId',_,machineTypeId,_,_,_,_) <- machinesQuery -< ()
  machineTypesRow <- join machineTypesQuery -< (machineTypeId)
  restrict -< (case machineId of
    Just(machineId'') -> (pgInt4 machineId'' .== machineId')
    Nothing -> pgBool True )
  returnA -< (machineRow, machineTypesRow)

machinesInCompanyByUpkeepQuery :: Int -> Query (DBInt, MachinesTable, MachineTypesTable)
machinesInCompanyByUpkeepQuery upkeepId = let
  companyPKQuery = limit 1 $ proc () -> do
    (_,_,machineFK,_) <- join upkeepMachinesQuery -< pgInt4 upkeepId
    (_,companyFK,_,_,_,_,_) <- join machinesQuery -< machineFK
    returnA -< companyFK
  in proc () -> do
    companyPK <- companyPKQuery -< ()
    m @ (_,companyFK,machineTypeFK,_,_,_,_) <- machinesQuery -< ()
    restrict -< (companyFK .== companyPK)
    mt <- join machineTypesQuery -< machineTypeFK
    returnA -< (companyPK, m, mt)

lastClosedMaintenanceQuery :: Int -> Query (UpkeepTable, UpkeepMachinesTable)
lastClosedMaintenanceQuery machineId = limit 1 $ orderBy (desc(\((_,date,_,_),_) -> date)) $ proc () -> do
  upkeepMachineRow @ (upkeepFK,_,_,_) <- join upkeepMachinesQuery -< pgInt4 machineId
  upkeepRow @ (_,_,upkeepClosed,_) <- join upkeepsQuery -< upkeepFK
  restrict -< pgBool True .== upkeepClosed
  returnA -< (upkeepRow, upkeepMachineRow)

nextMaintenanceQuery :: Int -> Query (UpkeepTable)
nextMaintenanceQuery machineId = limit 1 $ orderBy (asc(\(_,date,_,_) -> date)) $ proc () -> do
  upkeepRow @ (upkeepPK,_,upkeepClosed,_) <- upkeepsQuery -< ()
  restrict -< (upkeepClosed .== pgBool False)
  (_,_,machineFK,_) <- join upkeepMachinesQuery -< upkeepPK
  restrict -< pgInt4 machineId .== machineFK
  returnA -< upkeepRow

expandedUpkeepsQuery2 :: Int -> Query (UpkeepTable, UpkeepMachinesTable)
expandedUpkeepsQuery2 upkeepId = proc () -> do
  upkeepRow <- join upkeepsQuery -< pgInt4 upkeepId
  upkeepMachineRow <- join upkeepMachinesQuery -< pgInt4 upkeepId
  returnA -< (upkeepRow, upkeepMachineRow)

expandedUpkeepsQuery :: Query (UpkeepTable, UpkeepMachinesTable)
expandedUpkeepsQuery = proc () -> do
  upkeepRow @ (upkeepPK,_,_,_) <- upkeepsQuery -< ()
  upkeepMachineRow <- join upkeepMachinesQuery -< upkeepPK
  returnA -< (upkeepRow, upkeepMachineRow)

plannedUpkeepsQuery :: Query (UpkeepTable, CompaniesTable)
plannedUpkeepsQuery = proc () -> do
  upkeepRow @ (upkeepPK,_,upkeepClosed,_) <- upkeepsQuery -< ()
  restrict -< upkeepClosed .== pgBool False
  (_,_,machineFK,_) <- join upkeepMachinesQuery -< upkeepPK
  (_,companyFK,_,_,_,_,_) <- join machinesQuery -< machineFK
  companyRow <- join companiesQuery -< companyFK
  returnA -< (upkeepRow, companyRow)

groupedPlannedUpkeepsQuery :: Query (UpkeepTable, CompaniesTable)
groupedPlannedUpkeepsQuery = orderBy (asc(\((_,date,_,_), _) -> date)) $ 
  AGG.aggregate (p2 (p4(AGG.groupBy, AGG.min, AGG.boolOr, AGG.min),
    p6(AGG.min, AGG.min, AGG.min, AGG.min, AGG.min, AGG.min))) (plannedUpkeepsQuery)

runCompaniesQuery :: Connection -> IO [(Int, String, String, String, String, String)]
runCompaniesQuery connection = runQuery connection companiesQuery

singleMachineTypeQuery :: Either String Int -> Query MachineTypesTable
singleMachineTypeQuery machineTypeSid = proc () -> do
  machineTypeNameRow @ (mtId',name',_) <- machineTypesQuery -< ()
  restrict -< case machineTypeSid of
    Right(machineTypeId) -> (mtId' .== pgInt4 machineTypeId)
    Left(machineTypeName) -> (name' .== pgString machineTypeName)
  returnA -< machineTypeNameRow

runMachinesInCompanyQuery' :: Int -> Connection ->
  IO[((Int, Int, Int, Day, Int, Int, String), (Int, String, String))]
runMachinesInCompanyQuery' companyId connection =
  runQuery connection (machinesInCompanyQuery companyId)

runMachinesInCompanyQuery :: Int -> Connection -> IO[(Int, M.Machine, Int, Int, MT.MachineType)]
runMachinesInCompanyQuery companyId connection = do
  rows <- (runMachinesInCompanyQuery' companyId connection)
  return $ map convertExpanded rows

convertExpanded :: ((Int, Int, Int, Day, Int, Int, String),(Int, String, String)) 
                -> (Int, M.Machine, Int, Int, MT.MachineType)
convertExpanded = (\((mId,cId,_,mOs,m3,m4,m5),(mtId,mtN,mtMf)) ->
  (mId, M.Machine (dayToYmd mOs) m3 m4 m5, cId, mtId, (MT.MachineType mtN mtMf)))

runExpandedMachinesQuery' :: Maybe Int -> Connection 
  -> IO[((Int, Int, Int, Day, Int, Int, String), (Int, String, String))]
runExpandedMachinesQuery' machineId connection =
  runQuery connection (expandedMachinesQuery machineId)

runCompanyUpkeepsQuery :: Int -> Connection -> IO[((Int, Day, Bool, Maybe Int), (Maybe Int, Maybe String))]
runCompanyUpkeepsQuery companyId connection = 
  runQuery connection (companyUpkeepsQuery companyId)

runExpandedMachinesQuery :: Maybe Int -> Connection -> IO[(Int, M.Machine, Int, Int, MT.MachineType)]
runExpandedMachinesQuery machineId connection = do
  rows <- runExpandedMachinesQuery' machineId connection
  return $ map convertExpanded rows

runMachineTypesQuery' :: String -> Connection -> IO[String]
runMachineTypesQuery' mid connection = runQuery connection (machineTypesQuery' mid)

runLastClosedMaintenanceQuery :: Int -> Connection -> IO[((Int, Day, Bool, Maybe Int),(Int, String, Int, Int))]
runLastClosedMaintenanceQuery machineId connection =
  runQuery connection (lastClosedMaintenanceQuery machineId)

runNextMaintenanceQuery :: Int -> Connection -> IO[(Int, Day, Bool, Maybe Int)]
runNextMaintenanceQuery machineId connection = runQuery connection (nextMaintenanceQuery machineId)

runExpandedUpkeepsQuery :: Connection -> IO[((Int, Day, Bool, Maybe Int), (Int, String, Int, Int))]
runExpandedUpkeepsQuery connection = runQuery connection expandedUpkeepsQuery

runMachinesInCompanyByUpkeepQuery :: Int -> Connection -> IO[(Int, (Int, M.Machine, Int, Int, MT.MachineType))]
runMachinesInCompanyByUpkeepQuery upkeepId connection = do
  rows <- runQuery connection (machinesInCompanyByUpkeepQuery upkeepId)
  return $ map (\(companyId,a,b) -> (companyId, convertExpanded (a,b))) rows

runPlannedUpkeepsQuery :: Connection -> IO[((Int, Day, Bool, Maybe Int), (Int, String, String, String, String, String))]
runPlannedUpkeepsQuery connection = runQuery connection groupedPlannedUpkeepsQuery

runSingleUpkeepQuery :: Connection 
                     -> Int -- ^ upkeep id
                     -> IO[((Int, Day, Bool, Maybe Int), (Int, String, Int, Int))]
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


addCompany :: Connection -- ^ database connection
           -> C.Company -- ^ company to save in the db
           -> IO Int
addCompany connection newCompany = do
  newId <- runInsertReturning
    connection
    companiesTable (Nothing, pgString $ C.companyName newCompany, pgString $ C.companyPlant newCompany , pgString $ 
      C.companyAddress newCompany, pgString $ C.companyPerson newCompany, pgString $ C.companyPhone newCompany )
    sel1
  return $ head newId -- todo safe

singleRowOrColumn :: Monad m
                  => [a] 
                  -> ErrorT (Reason r) m a
singleRowOrColumn result = case result of
  row : xs | null xs -> return row
  _ -> throwError $ InputError $ ParseError "more than one record failure"

nextService :: Int 
            -> M.Machine 
            -> Int
            -> (a -> Connection) 
            -> ErrorT (Reason r) (ReaderT a IO) (D.YearMonthDay)
nextService machineId (M.Machine operationStartDate _ mileagePerYear _)
  machineTypeId getConn = ask >>= (\a -> do
  let conn = getConn a
  nextPlannedMaintenance <- liftIO $ runNextMaintenanceQuery machineId conn
  lastUpkeep <- liftIO $ runLastClosedMaintenanceQuery machineId conn
  actualUpkeepRepetition' <- liftIO $ (runQuery conn (actualUpkeepRepetitionQuery machineTypeId) :: IO [Int])
  actualUpkeepRepetition <- singleRowOrColumn actualUpkeepRepetition'
  nextDay <- let
    {- compute the next day, when the maintenance needs to be made -}
    compute :: Day -> Day
    compute lastServiceDay = let
      yearsToNextService = fromIntegral actualUpkeepRepetition / fromIntegral mileagePerYear :: Double
      daysToNextService = truncate $ yearsToNextService * 365
      nextServiceDay = addDays daysToNextService lastServiceDay
      in nextServiceDay
    in case (nextPlannedMaintenance, lastUpkeep) of
      -- next planned maintenance
      (x : xs,_) | null xs -> return $ sel2 x
      -- next maintenance computed from the last upkeep
      (_,(upkeep,_) : xs) | null xs -> return $ compute $ sel2 upkeep
      -- next maintenance computed from the operation start
      _ -> return $ compute (ymdToDay operationStartDate)
  return $ dayToYmd nextDay)
