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
import Opaleye.Column (Column)
import Opaleye.Order (orderBy, asc, limit, desc)
import Opaleye.RunQuery (runQuery)
import Opaleye.Operators ((.==), (.&&), restrict, lower)
import Opaleye.PGTypes (pgInt4, PGDate, pgDay, PGBool, PGInt4, PGText, pgString, pgBool)
import Opaleye.Manipulation (runInsert, runUpdate, runInsertReturning, runDelete)
import qualified Opaleye.Aggregate as AGG

import "mtl" Control.Monad.Reader (Reader, ReaderT, ask, runReaderT, mapReaderT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Error (ErrorT)
import Control.Monad.Trans.Class (lift)
import Control.Arrow (returnA)
import Control.Monad (forM_, forM)

import Data.Profunctor.Product (p2, p3, p4, p6)
import Data.JSON.Schema.Generic (gSchema)
import qualified Data.JSON.Schema.Types as JS (JSONSchema(schema))
import Data.Aeson.Types (toJSON, ToJSON, FromJSON, parseJSON)
import Data.Maybe (fromJust, maybeToList)
import Data.Functor.Identity (runIdentity)
import Data.Time.Calendar (Day, addDays)
import Data.Int (Int64)
import Data.List (intersperse)
import Data.Tuple.Select (Sel1, sel1)

import Rest.Api (Api, mkVersion, Some1(Some1), Router, route, root, compose)
import Rest.Resource (Resource, mkResourceId, Void, schema, list, name, create, mkResourceReaderWith, get ,
  update )
import qualified Rest.Schema as S
import Rest.Dictionary.Combinators (jsonO, someO, jsonI, someI)
import Rest.Handler (ListHandler, mkListing, mkInputHandler, Handler, mkConstHandler)
import Rest.Types.Error (DataError(ParseError),
  Reason(InputError, IdentError, NotFound, CustomReason, NotAllowed), DomainReason(DomainReason))

import qualified Crm.Shared.Company as C
import qualified Crm.Shared.Machine as M
import qualified Crm.Shared.MachineType as MT
import qualified Crm.Shared.Api as A
import qualified Crm.Shared.Upkeep as U
import qualified Crm.Shared.UpkeepMachine as UM
import qualified Crm.Shared.YearMonthDay as D
import Crm.Server.Helpers (ymdToDay, dayToYmd)

import Fay.Convert (showToFay, readFromFay')
import Fay.FFI (Nullable(Nullable, Null))

import Safe (readMay, minimumMay)
import Generics.Regular

import qualified Opaleye.Internal.Column as C
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

type DBInt = Column PGInt4
type DBText = Column PGText
type DBDate = Column PGDate
type DBBool = Column PGBool

type CompaniesTable = (DBInt, DBText, DBText)
type CompaniesWriteTable = (Maybe DBInt, DBText, DBText)

type MachinesTable = (DBInt, DBInt, DBInt, DBDate, DBInt, DBInt)
type MachinesWriteTable = (Maybe DBInt, DBInt, DBInt, DBDate, DBInt, DBInt)

type MachineTypesTable = (DBInt, DBText, DBText, DBInt)
type MachineTypesWriteTable = (Maybe DBInt, DBText, DBText, DBInt)

type UpkeepTable = (DBInt, DBDate, DBBool)
type UpkeepWriteTable = (Maybe DBInt, DBDate, DBBool)

type UpkeepMachinesTable = (DBInt, DBText, DBInt, DBInt)

companiesTable :: Table CompaniesWriteTable CompaniesTable
companiesTable = Table "companies" (p3 (
  optional "id" ,
  required "name" ,
  required "plant" ))

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
upkeepTable = Table "upkeeps" $ p3 (
  optional "id" ,
  required "date_" ,
  required "closed" )

upkeepMachinesTable :: Table UpkeepMachinesTable UpkeepMachinesTable
upkeepMachinesTable = Table "upkeep_machines" $ p4 (
  required "upkeep_id" ,
  required "note" ,
  required "machine_id" ,
  required "recorded_mileage" )

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

-- | joins table according with the id in
join :: (Sel1 a DBInt)
     => Query a
     -> QueryArr DBInt a
join tableQuery = proc id' -> do
  table <- tableQuery -< ()
  restrict -< sel1 table .== id'
  returnA -< table

runMachineUpdate :: (Int, Int, M.Machine) -> Connection -> IO Int64
runMachineUpdate (machineId', machineTypeId, machine') connection =
  runUpdate connection machinesTable readToWrite condition
    where
      condition (machineId,_,_,_,_,_) = machineId .== pgInt4 machineId'
      readToWrite  = const
        (Nothing, pgInt4 $ M.companyId machine', pgInt4 machineTypeId,
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
  c @ (companyPK,_,_) <- companiesQuery -< ()
  restrict -< (pgInt4 companyId .== companyPK)
  returnA -< c

runCompanyWithMachinesQuery :: Int -> Connection -> IO[(Int,String,String)]
runCompanyWithMachinesQuery companyId connection =
  runQuery connection (companyWithMachinesQuery companyId)

machinesInCompanyQuery :: Int -> Query (MachinesTable, MachineTypesTable)
machinesInCompanyQuery companyId = proc () -> do
  m @ (_,companyFK,machineTypeFK,_,_,_) <- machinesQuery -< ()
  mt <- join machineTypesQuery -< machineTypeFK
  restrict -< (pgInt4 companyId .== companyFK)
  returnA -< (m, mt)

companyUpkeepsQuery :: Int -> Query UpkeepTable
companyUpkeepsQuery companyId = let 
  upkeepsQuery' = proc () -> do
    (upkeepFK,_,machineFK,_) <- upkeepMachinesQuery -< ()
    (_,companyFK,_,_,_,_) <- join machinesQuery -< machineFK
    upkeep @ (_,_,closed) <- join upkeepQuery -< upkeepFK
    restrict -< (closed .== pgBool True)
    restrict -< (companyFK .== pgInt4 companyId)
    returnA -< upkeep
  aggregatedUpkeepsQuery = AGG.aggregate (p3(AGG.groupBy, AGG.min, AGG.boolOr)) upkeepsQuery'
  orderedUpkeepQuery = orderBy (asc(\(_,date,_) -> date)) $ aggregatedUpkeepsQuery
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
lastClosedMaintenanceQuery machineId = limit 1 $ orderBy (desc(\((_,date,_),_) -> date)) $ proc () -> do
  upkeepMachineRow @ (upkeepFK,_,_,_) <- join upkeepMachinesQuery -< pgInt4 machineId
  upkeepRow @ (_,_,upkeepClosed) <- join upkeepQuery -< upkeepFK
  restrict -< pgBool True .== upkeepClosed
  returnA -< (upkeepRow, upkeepMachineRow)

nextMaintenanceQuery :: Int -> Query (UpkeepTable)
nextMaintenanceQuery machineId = limit 1 $ orderBy (asc(\(_,date,_) -> date)) $ proc () -> do
  upkeepRow @ (upkeepPK,_,upkeepClosed) <- upkeepQuery -< ()
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
  upkeepRow @ (upkeepPK,_,_) <- upkeepQuery -< ()
  upkeepMachineRow <- join upkeepMachinesQuery -< upkeepPK
  returnA -< (upkeepRow, upkeepMachineRow)

plannedUpkeepsQuery :: Query (UpkeepTable, CompaniesTable)
plannedUpkeepsQuery = proc () -> do
  upkeepRow @ (upkeepPK,_,upkeepClosed) <- upkeepQuery -< ()
  restrict -< upkeepClosed .== pgBool False
  (_,_,machineFK,_) <- join upkeepMachinesQuery -< upkeepPK
  (_,companyFK,_,_,_,_) <- join machinesQuery -< machineFK
  companyRow <- join companiesQuery -< companyFK
  returnA -< (upkeepRow, companyRow)

groupedPlannedUpkeepsQuery :: Query (UpkeepTable, CompaniesTable)
groupedPlannedUpkeepsQuery = orderBy (asc(\((_,date,_), _) -> date)) $ 
  AGG.aggregate (p2 (p3(AGG.groupBy, AGG.min, AGG.boolOr),
    p3(AGG.min, AGG.min, AGG.min))) (plannedUpkeepsQuery)

runCompaniesQuery :: Connection -> IO [(Int, String, String)]
runCompaniesQuery connection = runQuery connection companiesQuery

runMachinesQuery :: Connection -> IO[(Int, Int, Int, Day, Int, Int)]
runMachinesQuery connection = runQuery connection machinesQuery

runMachineTypesQuery :: Connection -> IO[(Int, String, String, Int)]
runMachineTypesQuery connection = runQuery connection machineTypesQuery

singleMachineTypesQuery :: String -> Query MachineTypesTable
singleMachineTypesQuery machineTypeName = proc () -> do
  machineTypeNameRow @ (_,name',_,_) <- machineTypesQuery -< ()
  restrict -< (name' .== pgString machineTypeName)
  returnA -< machineTypeNameRow

runSingleMachineTypesQuery :: String -> Connection -> IO[(Int, String, String, Int)]
runSingleMachineTypesQuery machineTypeName connection = 
  runQuery connection (singleMachineTypesQuery machineTypeName)

runMachinesInCompanyQuery' :: Int -> Connection ->
  IO[((Int, Int, Int, Day, Int, Int), (Int, String, String, Int))]
runMachinesInCompanyQuery' companyId connection =
  runQuery connection (machinesInCompanyQuery companyId)

runMachinesInCompanyQuery :: Int -> Connection -> IO[(Int, M.Machine, Int, MT.MachineType)]
runMachinesInCompanyQuery companyId connection = do
  rows <- (runMachinesInCompanyQuery' companyId connection)
  return $ map convertExpanded rows

convertExpanded :: ((Int, Int, Int, Day, Int, Int),(Int, String, String, Int)) 
                -> (Int, M.Machine, Int, MT.MachineType)
convertExpanded = (\((mId,cId,_,mOs,m3,m4),(mtId,mtN,mtMf,mtI)) ->
  (mId, M.Machine cId (dayToYmd mOs) m3 m4, mtId, (MT.MachineType mtN mtMf mtI)))

runExpandedMachinesQuery' :: Maybe Int -> Connection 
  -> IO[((Int, Int, Int, Day, Int, Int), (Int, String, String, Int))]
runExpandedMachinesQuery' machineId connection =
  runQuery connection (expandedMachinesQuery machineId)

runCompanyUpkeepsQuery :: Int -> Connection -> IO[(Int, Day, Bool)]
runCompanyUpkeepsQuery companyId connection = 
  runQuery connection (companyUpkeepsQuery companyId)

runExpandedMachinesQuery :: Maybe Int -> Connection -> IO[(Int, M.Machine, Int, MT.MachineType)]
runExpandedMachinesQuery machineId connection = do
  rows <- runExpandedMachinesQuery' machineId connection
  return $ map convertExpanded rows

runMachineTypesQuery' :: String -> Connection -> IO[String]
runMachineTypesQuery' mid connection = runQuery connection (machineTypesQuery' mid)

runLastClosedMaintenanceQuery :: Int -> Connection -> IO[((Int, Day, Bool),(Int, String, Int, Int))]
runLastClosedMaintenanceQuery machineId connection =
  runQuery connection (lastClosedMaintenanceQuery machineId)

runNextMaintenanceQuery :: Int -> Connection -> IO[(Int, Day, Bool)]
runNextMaintenanceQuery machineId connection = runQuery connection (nextMaintenanceQuery machineId)

runExpandedUpkeepsQuery :: Connection -> IO[((Int, Day, Bool), (Int, String, Int, Int))]
runExpandedUpkeepsQuery connection = runQuery connection expandedUpkeepsQuery

runMachinesInCompanyByUpkeepQuery :: Int -> Connection -> IO[(Int, (Int, M.Machine, Int, MT.MachineType))]
runMachinesInCompanyByUpkeepQuery upkeepId connection = do
  rows <- runQuery connection (machinesInCompanyByUpkeepQuery upkeepId)
  return $ map (\(companyId,a,b) -> (companyId, convertExpanded (a,b))) rows

runPlannedUpkeepsQuery :: Connection -> IO[((Int, Day, Bool), (Int, String, String))]
runPlannedUpkeepsQuery connection = runQuery connection groupedPlannedUpkeepsQuery

runSingleUpkeepQuery :: Connection 
                     -> Int -- ^ upkeep id
                     -> IO[((Int, Day, Bool), (Int, String, Int, Int))]
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

type Dependencies = (ReaderT Connection IO :: * -> *)
type IdDependencies = (ReaderT (Connection, Either String Int) IO :: * -> *)
type StringIdDependencies = (ReaderT (Connection, String) IO :: * -> *)

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
  ask >>= (\conn -> forM rows (\(companyId, cName, cPlant) -> do
    machines <- liftIO $ runMachinesInCompanyQuery companyId conn
    nextDays <- forM machines (\(machineId, machine, _, machineType) -> do
      nextServiceDay <- nextService machineId machine machineType id
      return nextServiceDay )
    return $ (companyId, C.Company cName cPlant, maybeToList $ minimumMay nextDays))))

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

autocompleteSchema :: S.Schema String String Void
autocompleteSchema = S.noListing $ S.named [(
  "autocomplete", S.listingBy id ),(
  "by-type", S.singleBy id)]

companySchema :: S.Schema UrlId () Void
companySchema = S.withListing () $ S.unnamedSingle readMay'

addCompany :: Connection -- ^ database connection
           -> C.Company -- ^ company to save in the db
           -> IO Int
addCompany connection newCompany = do
  newId <- runInsertReturning
    connection
    companiesTable (Nothing, pgString $ C.companyName newCompany, pgString $ C.companyPlant newCompany)
    (\(id' ,_ ,_) -> id')
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

singleCompany :: Handler IdDependencies
singleCompany = mkConstHandler (jsonO . someO) (
  ask >>= \(conn, id') -> maybeId id' (\companyId -> do
    rows <- liftIO $ runCompanyWithMachinesQuery companyId conn
    (_,c2,c3) <- singleRowOrColumn rows
    machines <- liftIO $ runMachinesInCompanyQuery companyId conn
    return (C.Company c2 c3, machines)))

companyResource :: Resource Dependencies IdDependencies UrlId () Void
companyResource = (mkResourceReaderWith prepareReaderTuple) {
  list = const listing ,
  create = Just createCompanyHandler ,
  name = A.companies ,
  get = Just singleCompany ,
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
nextService machineId (M.Machine _ operationStartDate _ mileagePerYear) 
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
      ((_,date,_) : xs,_) | null xs -> return date
      -- next maintenance computed from the last upkeep
      (_,((_,date,_),_) : xs) | null xs -> return $ compute date
      -- next maintenance computed from the operation start
      _ -> return $ compute (ymdToDay operationStartDate)
  return $ dayToYmd nextDay)

machineSingle :: Handler IdDependencies
machineSingle = mkConstHandler (jsonO . someO) (
  ask >>= (\(conn,id') -> maybeId id' (\id'' -> do
    rows <- liftIO $ runExpandedMachinesQuery (Just id'') conn
    (machineId, machine, machineTypeId, machineType) <- singleRowOrColumn rows
    ymd <- nextService machineId machine machineType fst
    return (machine, machineTypeId, machineType, ymd))))

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

companyUpkeepsListing :: ListHandler IdDependencies
companyUpkeepsListing = mkListing (jsonO . someO) (const $
  ask >>= \(conn,id') -> maybeId id' (\id'' -> do
    rows <- liftIO $ runCompanyUpkeepsQuery id'' conn
    return $ map (\(id''',u1,u2) -> (id''', U.Upkeep (dayToYmd u1) [] u2)) rows))

machineTypesListing :: String -> ListHandler Dependencies
machineTypesListing mid = mkListing (jsonO . someO) (const $ 
  ask >>= \conn -> liftIO $ runMachineTypesQuery' mid conn )

machineTypesSingle :: Handler StringIdDependencies
machineTypesSingle = mkConstHandler (jsonO . someO) (
  ask >>= (\(conn,machineType) -> do
    rows <- liftIO $ runSingleMachineTypesQuery machineType conn
    idToMachineType <- case rows of
      (mtId, mtName, m3, m4) : xs | null xs -> return $ [ (mtId, MT.MachineType mtName m3 m4) ]
      [] -> return []
      _ -> throwError NotFound
    return idToMachineType))

upkeepsPlannedListing :: ListHandler Dependencies
upkeepsPlannedListing = mkListing (jsonO . someO) (const $ do
  rows <- ask >>= \conn -> liftIO $ runPlannedUpkeepsQuery conn
  let mappedRows = map (\((uPK,u2,u3),(cPK,c2,c3)) ->
        (uPK, U.Upkeep (dayToYmd u2) [] u3, cPK, C.Company c2 c3)) rows
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

mapUpkeeps :: [((Int, Day, Bool), (Int, String, Int, Int))] -> [(Int, U.Upkeep)]
mapUpkeeps rows = foldl (\acc ((upkeepId,date,upkeepClosed),(_,note,machineId,recordedMileage)) ->
  let
    addUpkeep' = (upkeepId, U.Upkeep (dayToYmd date) 
      [UM.UpkeepMachine note machineId recordedMileage] upkeepClosed)
    in case acc of
      [] -> [addUpkeep']
      (upkeepId', upkeep) : rest | upkeepId' == upkeepId -> let
        modifiedUpkeep = upkeep {
          U.upkeepMachines = UM.UpkeepMachine note machineId recordedMileage : U.upkeepMachines upkeep }
        in (upkeepId', modifiedUpkeep) : rest
      _ -> addUpkeep' : acc
  ) [] rows

upkeepListing :: ListHandler Dependencies
upkeepListing = mkListing (jsonO . someO) (const $ do
  rows <- ask >>= \conn -> liftIO $ runExpandedUpkeepsQuery conn
  return $ mapUpkeeps rows) 

insertUpkeepMachines :: Connection -> Int -> [UM.UpkeepMachine] -> IO ()
insertUpkeepMachines connection upkeepId upkeepMachines = let
  insertUpkeepMachine upkeepMachine' = do
    _ <- runInsert
      connection
      upkeepMachinesTable (
        pgInt4 upkeepId ,
        pgString $ UM.upkeepMachineNote upkeepMachine' ,
        pgInt4 $ UM.upkeepMachineMachineId upkeepMachine' ,
        pgInt4 $ UM.recordedMileage upkeepMachine' )
    return ()
  in forM_ upkeepMachines insertUpkeepMachine

updateUpkeep :: Connection
             -> Int
             -> U.Upkeep
             -> IO ()
updateUpkeep conn upkeepId upkeep = do
  _ <- let
    condition (upkeepId',_,_) = upkeepId' .== pgInt4 upkeepId
    readToWrite _ =
      (Nothing, pgDay $ ymdToDay $ U.upkeepDate upkeep, pgBool $ U.upkeepClosed upkeep)
    in runUpdate conn upkeepTable readToWrite condition
  _ <- runDelete conn upkeepMachinesTable (\(upkeepId',_,_,_) -> upkeepId' .== pgInt4 upkeepId)
  insertUpkeepMachines conn upkeepId (U.upkeepMachines upkeep)
  return ()

addUpkeep :: Connection
          -> U.Upkeep
          -> IO Int -- ^ id of the upkeep
addUpkeep connection upkeep = do
  upkeepIds <- runInsertReturning
    connection
    upkeepTable (Nothing, pgDay $ ymdToDay $ U.upkeepDate upkeep, pgBool $ U.upkeepClosed upkeep)
    (\(id',_,_) -> id')
  let upkeepId = head upkeepIds
  insertUpkeepMachines connection upkeepId (U.upkeepMachines upkeep)
  return upkeepId

addMachine :: Connection
           -> M.Machine
           -> MT.MyEither
           -> IO Int -- ^ id of newly created machine
addMachine connection machine machineType = do
  machineTypeId <- case machineType of
    MT.MyInt id' -> return $ id'
    MT.MyMachineType (MT.MachineType name' manufacturer upkeepPerMileage) -> do
      newMachineTypeId <- runInsertReturning
        connection
        machineTypesTable (Nothing, pgString name', pgString manufacturer, pgInt4 upkeepPerMileage)
        (\(id',_,_,_) -> id')
      return $ head newMachineTypeId -- todo safe
  let
    M.Machine companyId' machineOperationStartDate' initialMileage mileagePerYear = machine
  machineId <- runInsertReturning
    connection
    machinesTable (Nothing, pgInt4 companyId', pgInt4 machineTypeId, pgDay $ ymdToDay machineOperationStartDate',
      pgInt4 initialMileage, pgInt4 mileagePerYear)
    (\(id',_, _, _,_,_) -> id')
  return $ head machineId -- todo safe

createMachineHandler :: Handler IdDependencies
createMachineHandler = mkInputHandler (jsonO . jsonI . someI . someO) (\(newMachine,machineType) ->
  ask >>= \(connection, maybeInt) -> maybeId maybeInt (\companyId -> 
    liftIO $ addMachine connection newMachine machineType))

updateUpkeepHandler :: Handler IdDependencies
updateUpkeepHandler = mkInputHandler (jsonO . jsonI . someI . someO) (\upkeep ->
  ask >>= \(connection, maybeInt) -> maybeId maybeInt (\upkeepId ->
    liftIO $ updateUpkeep connection upkeepId upkeep))
    
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
  ask >>= \(connection, maybeInt) -> maybeId maybeInt (
    -- todo check that the machines are belonging to this company
    const $ liftIO $ addUpkeep connection newUpkeep))

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

machineTypeResource :: Resource Dependencies StringIdDependencies String String Void
machineTypeResource = (mkResourceReaderWith prepareReaderTuple) {
  name = A.machineTypes ,
  list = machineTypesListing ,
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

router' :: Router Dependencies Dependencies
router' = root `compose` ((route companyResource `compose` route companyMachineResource)
                                                 `compose` route upkeepResource)
               `compose` route machineResource
               `compose` route upkeepTopLevelResource
               `compose` route machineTypeResource

api :: Api Dependencies
api = [(mkVersion 1 0 0, Some1 $ router')]
