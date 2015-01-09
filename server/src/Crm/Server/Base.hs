{-# OPTIONS -fno-warn-orphans #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PackageImports #-}

{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Crm.Server.Base where

import Database.PostgreSQL.Simple (ConnectInfo(..), Connection, defaultConnectInfo, connect, close)

import Opaleye.QueryArr (Query, QueryArr)
import Opaleye.Table (Table(Table), required, queryTable, optional)
import Opaleye.Column (Column, toNullable, Nullable)
import qualified Opaleye.Column as COL
import Opaleye.Order (orderBy, asc, limit, desc)
import Opaleye.RunQuery (runQuery)
import Opaleye.Operators ((.==), (.&&), restrict, lower)
import Opaleye.PGTypes (pgInt4, PGDate, pgDay, PGBool, PGInt4, PGInt8, PGText, pgString, pgBool)
import Opaleye.Manipulation (runInsert, runUpdate, runInsertReturning, runDelete)
import Opaleye.Internal.RunQuery
import qualified Opaleye.Aggregate as AGG

import "mtl" Control.Monad.Reader (Reader, ReaderT, ask, runReaderT, mapReaderT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Error (ErrorT)
import Control.Monad.Trans.Class (lift)
import Control.Arrow (returnA)
import Control.Monad (forM_, forM)

import Data.Profunctor.Product (p1, p2, p3, p4, p6)
import Data.JSON.Schema.Generic (gSchema)
import qualified Data.JSON.Schema.Types as JS (JSONSchema(schema))
import Data.Aeson.Types (toJSON, ToJSON, FromJSON, parseJSON)
import Data.Maybe (fromJust, maybeToList)
import Data.Functor.Identity (runIdentity)
import Data.Time.Calendar (Day, addDays)
import Data.Int (Int64)
import Data.List (intersperse, sortBy)
import Data.Tuple.All (Sel1, sel1, sel2, sel3, upd3, uncurryN)

import Rest.Api (Api, mkVersion, Some1(Some1), Router, route, root, compose)
import Rest.Resource (Resource, mkResourceId, Void, schema, list, name, create, mkResourceReaderWith, get ,
  update )
import qualified Rest.Schema as S
import Rest.Dictionary.Combinators (jsonO, someO, jsonI, someI)
import Rest.Handler (ListHandler, mkListing, mkInputHandler, Handler, mkConstHandler)
import Rest.Types.Error (DataError(ParseError), Reason(InputError, IdentError, 
  NotFound, CustomReason, NotAllowed, UnsupportedRoute), DomainReason(DomainReason))

import qualified Crm.Shared.Company as C
import qualified Crm.Shared.Machine as M
import qualified Crm.Shared.MachineType as MT
import qualified Crm.Shared.Api as A
import qualified Crm.Shared.Upkeep as U
import qualified Crm.Shared.UpkeepMachine as UM
import qualified Crm.Shared.YearMonthDay as D
import qualified Crm.Shared.Employee as E
import qualified Crm.Shared.UpkeepSequence as US
import Crm.Server.Helpers (ymdToDay, dayToYmd)

import Fay.Convert (showToFay, readFromFay')

import Safe (readMay, minimumMay)
import Generics.Regular

import qualified Opaleye.Internal.Column as C
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

type DBInt = Column PGInt4
type DBInt8 = Column PGInt8
type DBText = Column PGText
type DBDate = Column PGDate
type DBBool = Column PGBool

type CompaniesTable = (DBInt, DBText, DBText, DBText, DBText, DBText)
type CompaniesWriteTable = (Maybe DBInt, DBText, DBText, DBText, DBText, DBText)

type MachinesTable = (DBInt, DBInt, DBInt, DBDate, DBInt, DBInt)
type MachinesWriteTable = (Maybe DBInt, DBInt, DBInt, DBDate, DBInt, DBInt)

type MachineTypesTable = (DBInt, DBText, DBText, DBInt)
type MachineTypesWriteTable = (Maybe DBInt, DBText, DBText, DBInt)

type UpkeepTable = (DBInt, DBDate, DBBool, Column (Nullable PGInt4))
type UpkeepWriteTable = (Maybe DBInt, DBDate, DBBool, (Column (Nullable PGInt4)))

type UpkeepMachinesTable = (DBInt, DBText, DBInt, DBInt)

type EmployeeTable = (DBInt, DBText)
type EmployeeWriteTable = (Maybe DBInt, DBText)

type UpkeepSequencesTable = (DBInt, DBText, DBInt, DBInt)

companiesTable :: Table CompaniesWriteTable CompaniesTable
companiesTable = Table "companies" (p6 (
  optional "id" ,
  required "name" ,
  required "plant" ,
  required "address" ,
  required "person" ,
  required "phone" ))

machinesTable :: Table MachinesWriteTable MachinesTable
machinesTable = Table "machines" (p6 (
  optional "id" ,
  required "company_id" ,
  required "machine_type_id" ,
  required "operation_start" ,
  required "initial_mileage" ,
  required "mileage_per_year" ))

machineTypesTable :: Table MachineTypesWriteTable MachineTypesTable
machineTypesTable = Table "machine_types" (p4 (
  optional "id" ,
  required "name" ,
  required "manufacturer" ,
  required "upkeep_per_mileage" ))

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
upkeepSequencesTable = Table "upkeep_sequences" $ p4 (
  required "display_ordering" ,
  required "label" ,
  required "repetition" ,
  required "machine_type_id" )

companiesQuery :: Query CompaniesTable
companiesQuery = queryTable companiesTable

machinesQuery :: Query MachinesTable
machinesQuery = queryTable machinesTable

machineTypesQuery :: Query MachineTypesTable
machineTypesQuery = queryTable machineTypesTable

upkeepQuery :: Query UpkeepTable
upkeepQuery = queryTable upkeepTable

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

runMachineUpdate :: (Int, Int, M.Machine) 
                 -> Connection 
                 -> IO Int64
runMachineUpdate (machineId', machineTypeId, machine') connection =
  runUpdate connection machinesTable readToWrite condition
    where
      condition (machineId,_,_,_,_,_) = machineId .== pgInt4 machineId'
      readToWrite (_,companyId,_,_,_,_) =
        (Nothing, companyId, pgInt4 machineTypeId,
          pgDay $ ymdToDay $ M.machineOperationStartDate machine',
          pgInt4 $ M.initialMileage machine', pgInt4 $ M.mileagePerYear machine')

like :: Column PGText -> Column PGText -> Column PGBool
like = C.binOp HPQ.OpLike

machineTypesQuery' :: String -> Query DBText
machineTypesQuery' mid = proc () -> do
  (_,name',_,_) <- machineTypesQuery -< ()
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
    (machinePK,_,machineTypeFK,_,_,_) <- machinesQuery -< ()
    mt <- join machineTypesQuery -< (machineTypeFK)
    returnA -< (mt, machinePK)
  aggregatedQuery = AGG.aggregate (p2(p4(AGG.groupBy, AGG.min, AGG.min, AGG.min),p1(AGG.count))) query
  orderedQuery = orderBy (asc(\((_,name',_,_),_) -> name')) aggregatedQuery
  in orderedQuery

machinesInCompanyQuery :: Int -> Query (MachinesTable, MachineTypesTable)
machinesInCompanyQuery companyId = orderBy (asc(\((machineId,_,_,_,_,_),_) -> machineId)) $ proc () -> do
  m @ (_,companyFK,machineTypeFK,_,_,_) <- machinesQuery -< ()
  mt <- join machineTypesQuery -< machineTypeFK
  restrict -< (pgInt4 companyId .== companyFK)
  returnA -< (m, mt)

companyUpkeepsQuery :: Int -> Query UpkeepTable
companyUpkeepsQuery companyId = let 
  upkeepsQuery' = proc () -> do
    (upkeepFK,_,machineFK,_) <- upkeepMachinesQuery -< ()
    (_,companyFK,_,_,_,_) <- join machinesQuery -< machineFK
    upkeep @ (_,_,closed,_) <- join upkeepQuery -< upkeepFK
    restrict -< (closed .== pgBool True)
    restrict -< (companyFK .== pgInt4 companyId)
    returnA -< upkeep
  aggregatedUpkeepsQuery = AGG.aggregate (p4(AGG.groupBy, AGG.min, AGG.boolOr, AGG.min)) upkeepsQuery'
  orderedUpkeepQuery = orderBy (asc(\(_,date,_,_) -> date)) $ aggregatedUpkeepsQuery
  in orderedUpkeepQuery

-- | query, that returns expanded machine type, not just the id
expandedMachinesQuery :: Maybe Int -> Query (MachinesTable, MachineTypesTable)
expandedMachinesQuery machineId = proc () -> do
  machineRow @ (machineId',_,machineTypeId,_,_,_) <- machinesQuery -< ()
  machineTypesRow @ (machineTypeId',_,_,_) <- machineTypesQuery -< ()
  let join = machineTypeId' .== machineTypeId
  restrict -< (case machineId of
    Just(machineId'') -> (pgInt4 machineId'' .== machineId') .&& join
    Nothing -> join)
  returnA -< (machineRow, machineTypesRow)

machinesInCompanyByUpkeepQuery :: Int -> Query (DBInt, MachinesTable, MachineTypesTable)
machinesInCompanyByUpkeepQuery upkeepId = let
  companyPKQuery = limit 1 $ proc () -> do
    (_,_,machineFK,_) <- join upkeepMachinesQuery -< pgInt4 upkeepId
    (_,companyFK,_,_,_,_) <- join machinesQuery -< machineFK
    returnA -< companyFK
  in proc () -> do
    companyPK <- companyPKQuery -< ()
    m @ (_,companyFK,machineTypeFK,_,_,_) <- machinesQuery -< ()
    restrict -< (companyFK .== companyPK)
    mt <- join machineTypesQuery -< machineTypeFK
    returnA -< (companyPK, m, mt)

lastClosedMaintenanceQuery :: Int -> Query (UpkeepTable, UpkeepMachinesTable)
lastClosedMaintenanceQuery machineId = limit 1 $ orderBy (desc(\((_,date,_,_),_) -> date)) $ proc () -> do
  upkeepMachineRow @ (upkeepFK,_,_,_) <- join upkeepMachinesQuery -< pgInt4 machineId
  upkeepRow @ (_,_,upkeepClosed,_) <- join upkeepQuery -< upkeepFK
  restrict -< pgBool True .== upkeepClosed
  returnA -< (upkeepRow, upkeepMachineRow)

nextMaintenanceQuery :: Int -> Query (UpkeepTable)
nextMaintenanceQuery machineId = limit 1 $ orderBy (asc(\(_,date,_,_) -> date)) $ proc () -> do
  upkeepRow @ (upkeepPK,_,upkeepClosed,_) <- upkeepQuery -< ()
  restrict -< (upkeepClosed .== pgBool False)
  (_,_,machineFK,_) <- join upkeepMachinesQuery -< upkeepPK
  restrict -< pgInt4 machineId .== machineFK
  returnA -< upkeepRow

expandedUpkeepsQuery2 :: Int -> Query (UpkeepTable, UpkeepMachinesTable)
expandedUpkeepsQuery2 upkeepId = proc () -> do
  upkeepRow <- join upkeepQuery -< pgInt4 upkeepId
  upkeepMachineRow <- join upkeepMachinesQuery -< pgInt4 upkeepId
  returnA -< (upkeepRow, upkeepMachineRow)

expandedUpkeepsQuery :: Query (UpkeepTable, UpkeepMachinesTable)
expandedUpkeepsQuery = proc () -> do
  upkeepRow @ (upkeepPK,_,_,_) <- upkeepQuery -< ()
  upkeepMachineRow <- join upkeepMachinesQuery -< upkeepPK
  returnA -< (upkeepRow, upkeepMachineRow)

plannedUpkeepsQuery :: Query (UpkeepTable, CompaniesTable)
plannedUpkeepsQuery = proc () -> do
  upkeepRow @ (upkeepPK,_,upkeepClosed,_) <- upkeepQuery -< ()
  restrict -< upkeepClosed .== pgBool False
  (_,_,machineFK,_) <- join upkeepMachinesQuery -< upkeepPK
  (_,companyFK,_,_,_,_) <- join machinesQuery -< machineFK
  companyRow <- join companiesQuery -< companyFK
  returnA -< (upkeepRow, companyRow)

groupedPlannedUpkeepsQuery :: Query (UpkeepTable, CompaniesTable)
groupedPlannedUpkeepsQuery = orderBy (asc(\((_,date,_,_), _) -> date)) $ 
  AGG.aggregate (p2 (p4(AGG.groupBy, AGG.min, AGG.boolOr, AGG.min),
    p6(AGG.min, AGG.min, AGG.min, AGG.min, AGG.min, AGG.min))) (plannedUpkeepsQuery)

runCompaniesQuery :: Connection -> IO [(Int, String, String, String, String, String)]
runCompaniesQuery connection = runQuery connection companiesQuery

runMachinesQuery :: Connection -> IO[(Int, Int, Int, Day, Int, Int)]
runMachinesQuery connection = runQuery connection machinesQuery

runMachineTypesQuery :: Connection -> IO[(Int, String, String, Int)]
runMachineTypesQuery connection = runQuery connection machineTypesQuery

singleMachineTypeQuery :: Either String Int -> Query MachineTypesTable
singleMachineTypeQuery machineTypeSid = proc () -> do
  machineTypeNameRow @ (mtId',name',_,_) <- machineTypesQuery -< ()
  restrict -< case machineTypeSid of
    Right(machineTypeId) -> (mtId' .== pgInt4 machineTypeId)
    Left(machineTypeName) -> (name' .== pgString machineTypeName)
  returnA -< machineTypeNameRow

runMachinesInCompanyQuery' :: Int -> Connection ->
  IO[((Int, Int, Int, Day, Int, Int), (Int, String, String, Int))]
runMachinesInCompanyQuery' companyId connection =
  runQuery connection (machinesInCompanyQuery companyId)

runMachinesInCompanyQuery :: Int -> Connection -> IO[(Int, M.Machine, Int, Int, MT.MachineType)]
runMachinesInCompanyQuery companyId connection = do
  rows <- (runMachinesInCompanyQuery' companyId connection)
  return $ map convertExpanded rows

convertExpanded :: ((Int, Int, Int, Day, Int, Int),(Int, String, String, Int)) 
                -> (Int, M.Machine, Int, Int, MT.MachineType)
convertExpanded = (\((mId,cId,_,mOs,m3,m4),(mtId,mtN,mtMf,mtI)) ->
  (mId, M.Machine (dayToYmd mOs) m3 m4, cId, mtId, (MT.MachineType mtN mtMf mtI)))

runExpandedMachinesQuery' :: Maybe Int -> Connection 
  -> IO[((Int, Int, Int, Day, Int, Int), (Int, String, String, Int))]
runExpandedMachinesQuery' machineId connection =
  runQuery connection (expandedMachinesQuery machineId)

runCompanyUpkeepsQuery :: Int -> Connection -> IO[(Int, Day, Bool, Maybe Int)]
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

data MachineTypeMid = Autocomplete String | CountListing
data MachineTypeSid = MachineTypeByName String | MachineTypeById (Either String Int)

type Dependencies = (ReaderT Connection IO :: * -> *)
type IdDependencies = (ReaderT (Connection, Either String Int) IO :: * -> *)
type StringIdDependencies = (ReaderT (Connection, String) IO :: * -> *)
type MachineTypeDependencies = (ReaderT (Connection, MachineTypeSid) IO :: * -> *)

deriveAll ''C.Company "PFCompany"
type instance PF C.Company = PFCompany

deriveAll ''M.Machine "PFMachine"
type instance PF M.Machine = PFMachine

deriveAll ''MT.MachineType "PFMachineType"
type instance PF MT.MachineType = PFMachineType

deriveAll ''U.Upkeep "PFUpkeep"
type instance PF U.Upkeep = PFUpkeep

deriveAll ''UM.UpkeepMachine "PFUpkeepMachine"
type instance PF UM.UpkeepMachine = PFUpkeepMachine

fayInstance value = case readFromFay' value of
  Left e -> fail e
  Right ok -> return ok
instance FromJSON MT.MyEither where
  parseJSON = fayInstance
instance FromJSON MT.MachineType where 
  parseJSON = fayInstance
instance FromJSON C.Company where
  parseJSON = fayInstance
instance FromJSON U.Upkeep where
  parseJSON = fayInstance
instance FromJSON UM.UpkeepMachine where
  parseJSON = fayInstance
instance FromJSON M.Machine where
  parseJSON = fayInstance

-- super unsafe
instance ToJSON D.YearMonthDay where
  toJSON = fromJust . showToFay
instance ToJSON C.Company where
  toJSON = fromJust . showToFay
instance ToJSON U.Upkeep where
  toJSON = fromJust . showToFay
instance ToJSON MT.MachineType where
  toJSON = fromJust . showToFay
instance ToJSON M.Machine where
  toJSON = fromJust . showToFay
instance ToJSON UM.UpkeepMachine where
  toJSON = fromJust . showToFay
instance ToJSON E.Employee where
  toJSON = fromJust . showToFay

instance JS.JSONSchema E.Employee where
  schema = gSchema
instance JS.JSONSchema C.Company where
  schema = gSchema
instance JS.JSONSchema M.Machine where
  schema = gSchema
instance JS.JSONSchema D.YearMonthDay where
  schema = gSchema
instance JS.JSONSchema D.Precision where
  schema = gSchema
instance JS.JSONSchema MT.MachineType where
  schema = gSchema
instance JS.JSONSchema Char where
  schema = gSchema
instance JS.JSONSchema MT.MyEither where
  schema = gSchema
instance JS.JSONSchema U.Upkeep where
  schema = gSchema
instance JS.JSONSchema UM.UpkeepMachine where
  schema = gSchema
instance JS.JSONSchema US.UpkeepSequence where
  schema = gSchema

instance Eq D.YearMonthDay where
  D.YearMonthDay y m d _ == D.YearMonthDay y' m' d' _ = y == y' && m == m' && d == d'
instance Ord D.YearMonthDay where
  ymd1 `compare` ymd2 = let
    D.YearMonthDay y m d _ = ymd1
    D.YearMonthDay y' m' d' _ = ymd2
    comp comparison nextComparison = case comparison of
      GT -> GT
      LT -> LT
      EQ -> nextComparison
    in comp (y `compare` y') $ comp (m `compare` m') $ comp (d `compare` d') EQ

listing :: ListHandler Dependencies
listing = mkListing (jsonO . someO) (const $ do
  rows <- ask >>= \conn -> liftIO $ runCompaniesQuery conn
  ask >>= (\conn -> do 
    unsortedResult <- forM rows (\companyRow -> do
      let companyId = sel1 companyRow
      machines <- liftIO $ runMachinesInCompanyQuery companyId conn
      nextDays <- forM machines (\(machineId, machine, _, _, machineType) -> do
        nextServiceDay <- nextService machineId machine machineType id
        return nextServiceDay )
      return $ (companyId, (uncurryN $ const C.Company) companyRow , maybeToList $ minimumMay nextDays))
    return $ sortBy (\r1 r2 -> let 
      date1 = sel3 r1  
      date2 = sel3 r2
      in case (date1, date2) of
        (date1' : _, date2' : _) -> date1' `compare` date2'
        ([],[]) -> EQ
        ([],_) -> GT
        _ -> LT ) unsortedResult ))

type UrlId = Either String Int 

schema' :: S.Schema Void () Void
schema' = S.withListing () (S.named [])

data UpkeepsListing = UpkeepsAll | UpkeepsPlanned

upkeepSchema :: S.Schema UrlId UpkeepsListing Void
upkeepSchema = S.withListing UpkeepsAll (S.named [
  ("planned", S.listing UpkeepsPlanned) ,
  ("single", S.singleBy readMay') ])

readMay' :: (Read a) => String -> Either String a
readMay' string = passStringOnNoRead $ readMay string
  where
    passStringOnNoRead (Just parsed) = Right parsed
    passStringOnNoRead _ = Left string

schema'' :: S.Schema UrlId () Void
schema'' = S.withListing () (S.unnamedSingle readMay')

autocompleteSchema :: S.Schema MachineTypeSid MachineTypeMid Void
autocompleteSchema = S.withListing CountListing $ S.named [(
  "autocomplete", S.listingBy (\str -> Autocomplete str)),(
  A.byName, S.singleBy (\str -> MachineTypeByName str)),(
  "by-id", S.singleBy (\mtId -> MachineTypeById $ readMay' mtId))]

companySchema :: S.Schema UrlId () Void
companySchema = S.withListing () $ S.unnamedSingle readMay'

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

createCompanyHandler :: Handler Dependencies
createCompanyHandler = mkInputHandler (jsonO . jsonI . someI . someO) (\newCompany ->
  ask >>= \conn -> liftIO $ addCompany conn newCompany )

prepareReaderIdentity :: ReaderT (b, c) IO a
                      -> ReaderT c (ReaderT (b, c) IO) a
prepareReaderIdentity = prepareReader (\c (b, _) -> (b, c))

prepareReader :: (c -> d -> b)
              -> ReaderT b IO a
              -> ReaderT c (ReaderT d IO) a
prepareReader constructB reader = 
  mapReaderT (\cIdentity -> let
    cc = runIdentity cIdentity
    innerReader = ask >>= (\dd -> let
      constructedB = constructB cc dd
      aa = runReaderT reader constructedB
      in lift aa)
    in innerReader) outerReader
  where
    outerReader = ask

prepareReaderTuple :: ReaderT (c, b) IO a
                   -> ReaderT b (ReaderT c IO) a
prepareReaderTuple = prepareReader (\b c -> (c, b))

updateCompany :: Handler IdDependencies
updateCompany = mkInputHandler (jsonI . someI . jsonO . someO) (\company ->
  ask >>= \(conn, companyId') -> maybeId companyId' (\companyId -> do
    let
      readToWrite = const (Nothing, pgString $ C.companyName company, pgString $ C.companyPlant company ,
        pgString $ C.companyAddress company, pgString $ C.companyPerson company, pgString $ C.companyPhone company)
      condition = (pgInt4 companyId .==) . sel1
    _ <- liftIO $ runUpdate conn companiesTable readToWrite condition
    return ()))
    

singleCompany :: Handler IdDependencies
singleCompany = mkConstHandler (jsonO . someO) (
  ask >>= \(conn, id') -> maybeId id' (\companyId -> do
    rows <- liftIO $ runCompanyWithMachinesQuery companyId conn
    company <- singleRowOrColumn rows
    machines <- liftIO $ runMachinesInCompanyQuery companyId conn
    return ((uncurryN (const C.Company)) company , machines)))

companyResource :: Resource Dependencies IdDependencies UrlId () Void
companyResource = (mkResourceReaderWith prepareReaderTuple) {
  list = const listing ,
  create = Just createCompanyHandler ,
  name = A.companies ,
  get = Just singleCompany ,
  update = Just updateCompany ,
  schema = companySchema }

machineUpdate :: Handler IdDependencies
machineUpdate = mkInputHandler (jsonI . someI) (\(machineTypeId, machine) ->
  ask >>= \(conn, id') -> maybeId id' (\machineId -> do
    _ <- liftIO $ runMachineUpdate (machineId, machineTypeId, machine) conn
    -- todo singal error if the update didn't hit a row
    return ()))

nextService :: Int 
            -> M.Machine 
            -> MT.MachineType 
            -> (a -> Connection) 
            -> ErrorT (Reason r) (ReaderT a IO) (D.YearMonthDay)
nextService machineId (M.Machine operationStartDate _ mileagePerYear) 
  (MT.MachineType _ _ upkeepPerMileage) getConn = ask >>= (\a -> do
  let conn = getConn a
  nextPlannedMaintenance <- liftIO $ runNextMaintenanceQuery machineId conn
  lastUpkeep <- liftIO $ runLastClosedMaintenanceQuery machineId conn
  nextDay <- let
    {- compute the next day, when the maintenance needs to be made -}
    compute :: Day -> Day
    compute lastServiceDay = let
      yearsToNextService = fromIntegral upkeepPerMileage / fromIntegral mileagePerYear
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

machineSingle :: Handler IdDependencies
machineSingle = mkConstHandler (jsonO . someO) (
  ask >>= (\(conn,id') -> maybeId id' (\id'' -> do
    rows <- liftIO $ runExpandedMachinesQuery (Just id'') conn
    (machineId, machine, companyId, machineTypeId, machineType) <- singleRowOrColumn rows
    ymd <- nextService machineId machine machineType fst
    return (machine, companyId, machineTypeId, machineType, ymd))))

machineListing :: ListHandler Dependencies
machineListing = mkListing (jsonO . someO) (const $ do
  ask >>= \conn -> liftIO $ runExpandedMachinesQuery Nothing conn)

maybeId :: Monad b
        => Either String Int 
        -> (Int -> ErrorT (Reason r) b a)
        -> ErrorT (Reason r) b a
maybeId maybeInt onSuccess = case maybeInt of
  Right(int) -> onSuccess int
  Left(string) -> throwError $ IdentError $ ParseError
    ("provided identificator(" ++ string ++ ") cannot be parsed into number.")

employeesListing :: ListHandler Dependencies 
employeesListing = mkListing (jsonO . someO) (const $
  ask >>= \conn -> do 
    rawRows <- liftIO $ runQuery conn employeesQuery
    let rowsMapped = map (\(eId,employeeName) -> (eId :: Int, E.Employee employeeName)) rawRows 
    return rowsMapped )

companyUpkeepsListing :: ListHandler IdDependencies
companyUpkeepsListing = mkListing (jsonO . someO) (const $
  ask >>= \(conn,id') -> maybeId id' (\id'' -> do
    rows <- liftIO $ runCompanyUpkeepsQuery id'' conn
    return $ map (\(id''',u1,u2,_) -> (id''', U.Upkeep (dayToYmd u1) u2)) rows))

machineTypesListing :: MachineTypeMid -> ListHandler Dependencies
machineTypesListing (Autocomplete mid) = mkListing (jsonO . someO) (const $ 
  ask >>= \conn -> liftIO $ runMachineTypesQuery' mid conn )
machineTypesListing CountListing = mkListing (jsonO . someO) (const $ do
  rows <- ask >>= \conn -> liftIO $ runQuery conn machineTypesWithCountQuery 
  let 
    mapRow :: ((Int,String,String,Int),Int64) -> ((Int, MT.MachineType), Int)
    mapRow ((m1,m2,m3,m4),count) = ((m1, MT.MachineType m2 m3 m4), fromIntegral count)
    mappedRows = map mapRow rows
  return mappedRows )

updateMachineType :: Handler MachineTypeDependencies
updateMachineType = mkInputHandler (jsonO . jsonI . someI . someO) (\machineType ->
  ask >>= \(conn, sid) -> case sid of
    MachineTypeByName _ -> throwError UnsupportedRoute
    MachineTypeById machineTypeId' -> maybeId machineTypeId' (\machineTypeId -> liftIO $ let
      readToWrite = const (Nothing, pgString $ MT.machineTypeName machineType, 
        pgString $ MT.machineTypeManufacturer machineType, pgInt4 $ MT.upkeepPerMileage machineType)
      condition machineTypeRow = sel1 machineTypeRow .== pgInt4 machineTypeId
      in runUpdate conn machineTypesTable readToWrite condition ))

machineTypesSingle :: Handler MachineTypeDependencies
machineTypesSingle = mkConstHandler (jsonO . someO) (
  ask >>= (\(conn,machineTypeSid) -> do
    let 
      performQuery parameter = liftIO $ runQuery conn (singleMachineTypeQuery parameter)
      (onEmptyResult, result) = case machineTypeSid of
        MachineTypeById(Right(mtId)) -> (throwError NotFound, performQuery $ Right mtId)
        MachineTypeById(Left(_)) -> (undefined, throwError NotFound)
        MachineTypeByName(mtName) -> (return [], performQuery $ Left mtName)
    rows <- result
    idToMachineType <- case rows of
      (mtId, mtName, m3, m4) : xs | null xs -> return $ [ (mtId :: Int, MT.MachineType mtName m3 m4) ]
      [] -> onEmptyResult
      _ -> throwError NotFound
    return idToMachineType))

upkeepsPlannedListing :: ListHandler Dependencies
upkeepsPlannedListing = mkListing (jsonO . someO) (const $ do
  rows <- ask >>= \conn -> liftIO $ runPlannedUpkeepsQuery conn
  let mappedRows = map (\((uPK,u2,u3,_),companyRow) ->
        (uPK, U.Upkeep (dayToYmd u2) u3, sel1 companyRow, (uncurryN $ const C.Company) companyRow)) rows
  return mappedRows )

singleRowOrColumn :: Monad m
                  => [a] 
                  -> ErrorT (Reason r) m a
singleRowOrColumn result = case result of
  row : xs | null xs -> return row
  _ -> throwError $ InputError $ ParseError "more than one record failure"

getUpkeep :: Handler IdDependencies
getUpkeep = mkConstHandler (jsonO . someO) ( do
  rows <- ask >>= \(conn, upkeepId') -> maybeId upkeepId' (\upkeepId ->
    liftIO $ runSingleUpkeepQuery conn upkeepId)
  let result = mapUpkeeps rows
  singleRowOrColumn (map snd result))

mapUpkeeps :: [((Int, Day, Bool, Maybe Int), (Int, String, Int, Int))] -> [(Int, (U.Upkeep, Maybe Int, [(UM.UpkeepMachine, Int)]))]
mapUpkeeps rows = foldl (\acc ((upkeepId,date,upkeepClosed,employeeId),(_,note,machineId,recordedMileage)) ->
  let
    addUpkeep' = (upkeepId, (U.Upkeep (dayToYmd date) upkeepClosed, employeeId, 
      [(UM.UpkeepMachine note recordedMileage, machineId)]) )
    in case acc of
      [] -> [addUpkeep']
      (upkeepId', (upkeep, e, upkeepMachines)) : rest | upkeepId' == upkeepId -> let
        modifiedUpkeepMachines = 
          (UM.UpkeepMachine note recordedMileage, machineId) : upkeepMachines
        in (upkeepId', (upkeep, e, modifiedUpkeepMachines)) : rest
      _ -> addUpkeep' : acc
  ) [] rows

upkeepListing :: ListHandler Dependencies
upkeepListing = mkListing (jsonO . someO) (const $ do
  rows <- ask >>= \conn -> liftIO $ runExpandedUpkeepsQuery conn
  return $ mapUpkeeps rows) 

insertUpkeepMachines :: Connection -> Int -> [(UM.UpkeepMachine, Int)] -> IO ()
insertUpkeepMachines connection upkeepId upkeepMachines = let
  insertUpkeepMachine (upkeepMachine', upkeepMachineId) = do
    _ <- runInsert
      connection
      upkeepMachinesTable (
        pgInt4 upkeepId ,
        pgString $ UM.upkeepMachineNote upkeepMachine' ,
        pgInt4 upkeepMachineId ,
        pgInt4 $ UM.recordedMileage upkeepMachine' )
    return ()
  in forM_ upkeepMachines insertUpkeepMachine

-- type UpkeepWriteTable = (Maybe DBInt, DBDate, DBBool, Maybe (Column (Nullable PGInt4)))

maybeToNullable :: Maybe (Column a) -> Column (Nullable a)
maybeToNullable (Just a) = toNullable a
maybeToNullable Nothing = COL.null

updateUpkeep :: Connection
             -> Int
             -> (U.Upkeep, [(UM.UpkeepMachine, Int)], Maybe Int)
             -> IO ()
updateUpkeep conn upkeepId (upkeep, upkeepMachines, employeeId) = do
  _ <- let
    condition (upkeepId',_,_,_) = upkeepId' .== pgInt4 upkeepId
    readToWrite _ =
      (Nothing, pgDay $ ymdToDay $ U.upkeepDate upkeep, pgBool $ U.upkeepClosed upkeep, maybeToNullable $ fmap pgInt4 employeeId)
    in runUpdate conn upkeepTable readToWrite condition
  _ <- runDelete conn upkeepMachinesTable (\(upkeepId',_,_,_) -> upkeepId' .== pgInt4 upkeepId)
  insertUpkeepMachines conn upkeepId upkeepMachines
  return ()

addUpkeep :: Connection
          -> (U.Upkeep, [(UM.UpkeepMachine, Int)], Maybe Int)
          -> IO Int -- ^ id of the upkeep
addUpkeep connection (upkeep, upkeepMachines, employeeId) = do
  upkeepIds <- runInsertReturning
    connection
    upkeepTable (Nothing, pgDay $ ymdToDay $ U.upkeepDate upkeep, 
      pgBool $ U.upkeepClosed upkeep, maybeToNullable $ fmap pgInt4 employeeId)
    sel1
  let upkeepId = head upkeepIds
  insertUpkeepMachines connection upkeepId upkeepMachines
  return upkeepId

addMachine :: Connection
           -> M.Machine
           -> Int
           -> MT.MyEither
           -> IO Int -- ^ id of newly created machine
addMachine connection machine companyId' machineType = do
  machineTypeId <- case machineType of
    MT.MyInt id' -> return $ id'
    MT.MyMachineType (MT.MachineType name' manufacturer upkeepPerMileage, upkeepSequences) -> do
      newMachineTypeId <- runInsertReturning
        connection
        machineTypesTable (Nothing, pgString name', pgString manufacturer, pgInt4 upkeepPerMileage)
        sel1
      let machineTypeId = head newMachineTypeId -- todo safe
      forM_ upkeepSequences (\(US.UpkeepSequence displayOrdering label repetition) -> runInsert
        connection
        upkeepSequencesTable (pgInt4 displayOrdering, pgString label, pgInt4 repetition, pgInt4 machineTypeId))
      return machineTypeId
  let
    M.Machine machineOperationStartDate' initialMileage mileagePerYear = machine
  machineId <- runInsertReturning
    connection
    machinesTable (Nothing, pgInt4 companyId', pgInt4 machineTypeId, pgDay $ ymdToDay machineOperationStartDate',
      pgInt4 initialMileage, pgInt4 mileagePerYear)
    (\(id',_, _, _,_,_) -> id')
  return $ head machineId -- todo safe

createMachineHandler :: Handler IdDependencies
createMachineHandler = mkInputHandler (jsonO . jsonI . someI . someO) (\(newMachine,machineType) ->
  ask >>= \(connection, maybeInt) -> maybeId maybeInt (\companyId -> 
    liftIO $ addMachine connection newMachine companyId machineType))

updateUpkeepHandler :: Handler IdDependencies
updateUpkeepHandler = mkInputHandler (jsonO . jsonI . someI . someO) (\(upkeep,machines,employeeId) -> let 
  employeeListToMaybe = case employeeId of
    x : _ -> Just x
    _ -> Nothing
  upkeepTriple = (upkeep, machines, employeeListToMaybe)
  in ask >>= \(connection, maybeInt) -> maybeId maybeInt (\upkeepId ->
    liftIO $ updateUpkeep connection upkeepId upkeepTriple))
    
upkeepCompanyMachines :: Handler IdDependencies
upkeepCompanyMachines = mkConstHandler (jsonO . someO) (
  ask >>= \(conn, maybeUpkeepId) -> maybeId maybeUpkeepId (\upkeepId -> do
    upkeeps <- liftIO $ fmap mapUpkeeps (runSingleUpkeepQuery conn upkeepId)
    upkeep <- singleRowOrColumn upkeeps
    machines <- liftIO $ runMachinesInCompanyByUpkeepQuery upkeepId conn
    companyId <- case machines of
      [] -> throwError $ NotAllowed
      (companyId',_) : _ -> return companyId'
    return (companyId, snd upkeep, map snd machines)))

createUpkeepHandler :: Handler IdDependencies
createUpkeepHandler = mkInputHandler (jsonO . jsonI . someI . someO) (\newUpkeep ->
  let 
    (_,_,selectedEmployeeId) = newUpkeep
    employeeListToMaybe = case selectedEmployeeId of
      x : _ -> Just x
      _ -> Nothing
    newUpkeep' = upd3 employeeListToMaybe newUpkeep
    in ask >>= \(connection, maybeInt) -> maybeId maybeInt (
      -- todo check that the machines are belonging to this company
      const $ liftIO $ addUpkeep connection newUpkeep'))

companyMachineResource :: Resource IdDependencies IdDependencies Void Void Void
companyMachineResource = mkResourceId {
  name = A.machines ,
  schema = S.noListing $ S.named [] ,
  create = Just createMachineHandler }

machineResource :: Resource Dependencies IdDependencies UrlId () Void
machineResource = (mkResourceReaderWith prepareReaderTuple) {
  list = const machineListing ,
  get = Just machineSingle ,
  update = Just machineUpdate ,
  name = A.machines ,
  schema = schema'' }

machineTypeResource :: Resource Dependencies MachineTypeDependencies MachineTypeSid MachineTypeMid Void
machineTypeResource = (mkResourceReaderWith prepareReaderTuple) {
  name = A.machineTypes ,
  list = machineTypesListing ,
  update = Just updateMachineType ,
  get = Just machineTypesSingle ,
  schema = autocompleteSchema }

upkeepTopLevelResource :: Resource Dependencies IdDependencies UrlId UpkeepsListing Void
upkeepTopLevelResource = (mkResourceReaderWith prepareReaderTuple) {
  list = \listingType -> case listingType of
    UpkeepsAll -> upkeepListing
    UpkeepsPlanned -> upkeepsPlannedListing ,
  name = A.upkeep ,
  schema = upkeepSchema ,
  get = Just upkeepCompanyMachines }

upkeepResource :: Resource IdDependencies IdDependencies UrlId () Void
upkeepResource = (mkResourceReaderWith prepareReaderIdentity) {
  name = A.upkeep ,
  schema = S.withListing () $ S.unnamedSingle readMay' ,
  list = const $ companyUpkeepsListing ,
  get = Just getUpkeep ,
  update = Just updateUpkeepHandler ,
  create = Just createUpkeepHandler }

employeeResource :: Resource Dependencies Dependencies Void () Void
employeeResource = mkResourceId {
  name = A.employees ,
  schema = S.withListing () $ S.named [] ,
  list = const $ employeesListing }


router' :: Router Dependencies Dependencies
router' = root `compose` ((route companyResource `compose` route companyMachineResource)
                                                 `compose` route upkeepResource)
               `compose` route machineResource
               `compose` route upkeepTopLevelResource
               `compose` route machineTypeResource
               `compose` route employeeResource

api :: Api Dependencies
api = [(mkVersion 1 0 0, Some1 $ router')]
