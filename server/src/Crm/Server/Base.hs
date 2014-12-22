{-# OPTIONS -fno-warn-orphans #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PackageImports #-}

{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Crm.Server.Base where

import Opaleye.QueryArr (Query)
import Opaleye.Table (Table(Table), required, queryTable, optional)
import Opaleye.Column (Column)

import Data.Profunctor.Product (p2, p3, p4, p6)

import Database.PostgreSQL.Simple (ConnectInfo(..), Connection, defaultConnectInfo, connect, close)
import Opaleye.RunQuery (runQuery)
import Opaleye.Operators ((.==), (.&&), restrict)
import Opaleye.PGTypes (pgInt4, PGDate, pgDay, PGBool, PGInt4, PGText, pgString, pgBool)
import Opaleye.Manipulation (runInsert, runUpdate, runInsertReturning)
import qualified Opaleye.Aggregate as AGG

import "mtl" Control.Monad.Reader (Reader, ReaderT, ask, withReaderT, runReaderT, mapReaderT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans.Class (lift)
import Control.Arrow (returnA)
import Control.Monad (forM_)

import Data.JSON.Schema.Generic (gSchema)
import qualified Data.JSON.Schema.Types as JS (JSONSchema(schema))
import Data.Aeson.Types (toJSON, ToJSON, FromJSON, parseJSON)
import Data.Maybe (fromJust)
import Data.Functor.Identity (runIdentity)
import Data.Time.Calendar (Day)
import Data.Int (Int64)

import Rest.Api (Api, mkVersion, Some1(Some1), Router, route, root, compose)
import Rest.Resource (Resource, mkResourceId, Void, schema, list, name, create, mkResourceReaderWith, get ,
  update )
import qualified Rest.Schema as S
import Rest.Dictionary.Combinators (jsonO, someO, jsonI, someI, someE, jsonE)
import Rest.Handler (ListHandler, mkListing, mkInputHandler, Handler, mkConstHandler, mkIdHandler, mkHandler)
import Rest.Types.Error (DataError(ParseError), Reason(InputError, IdentError))

import Generics.Regular

import qualified Crm.Shared.Company as C
import qualified Crm.Shared.Machine as M
import qualified Crm.Shared.MachineType as MT
import qualified Crm.Shared.Api as A
import qualified Crm.Shared.Upkeep as U
import qualified Crm.Shared.UpkeepMachine as UM
import qualified Crm.Shared.YearMonthDay as D
import Crm.Server.Helpers (ymdToDay, dayToYmd)
import Fay.Convert (showToFay, readFromFay')

import Safe (readMay)
import Debug.Trace

type CompaniesTable = (Column PGInt4, Column PGText, Column PGText)
type CompaniesWriteTable = (Maybe (Column PGInt4), Column PGText, Column PGText)

type MachinesTable = (DBInt, DBInt, DBInt, DBDate, DBInt, DBInt)
type MachinesWriteTable = (Maybe DBInt, DBInt, DBInt, DBDate, DBInt, DBInt)

type MachineTypesTable = (Column PGInt4, Column PGText, Column PGText, DBInt)
type MachineTypesWriteTable = (Maybe (Column PGInt4), Column PGText, Column PGText, DBInt)

type DBInt = Column PGInt4
type DBText = Column PGText
type DBDate = Column PGDate
type DBBool = Column PGBool

type UpkeepTable = (DBInt, DBDate, DBBool)
type UpkeepWriteTable = (Maybe DBInt, DBDate, DBBool)

type UpkeepMachinesTable = (DBInt, DBText, DBInt)

companiesTable :: Table CompaniesWriteTable CompaniesTable
companiesTable = Table "companies" (p3 (
    optional "id"
    , required "name"
    , required "plant"
  ))

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
upkeepMachinesTable = Table "upkeep_machines" $ p3 (
  required "upkeep_id" ,
  required "note" ,
  required "machine_id")

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

runMachineUpdate :: (Int, M.Machine) -> Connection -> IO Int64
runMachineUpdate (machineId', machine') connection = 
  runUpdate connection machinesTable readToWrite condition
    where
      condition (machineId,_,_,_,_,_) = machineId .== pgInt4 machineId'
      readToWrite (_,_,machineTypeId,_,_,_) =
        (Nothing, pgInt4 $ M.companyId machine', machineTypeId, 
          pgDay $ ymdToDay $ M.machineOperationStartDate machine', 
          pgInt4 $ M.initialMileage machine', pgInt4 $ M.mileagePerYear machine')

-- | query, that returns expanded machine type, not just the id
expandedMachinesQuery :: Maybe Int -> Query (MachinesTable, MachineTypesTable)
expandedMachinesQuery machineId = proc () -> do
  machineRow @ (machineId',_,machineTypeId,_,_,_) <- machinesQuery -< ()
  machineTypesRow @ (machineTypeId',_,_,_) <- machineTypesQuery -< ()
  let join = machineTypeId' .== machineTypeId
  restrict -< (case machineId of
    Just(machineId'') -> (pgInt4 machineId'' .== machineId') .&& join
    Nothing -> join )
  returnA -< (machineRow, machineTypesRow)

expandedUpkeepsQuery :: Query (UpkeepTable, UpkeepMachinesTable)
expandedUpkeepsQuery = proc () -> do
  upkeepRow @ (upkeepId,_,_) <- upkeepQuery -< ()
  upkeepMachineRow @ (upkeepId',_,_) <- upkeepMachinesQuery -< ()
  restrict -< (upkeepId' .== upkeepId)
  returnA -< (upkeepRow, upkeepMachineRow)

plannedUpkeepsQuery :: Query (UpkeepTable, CompaniesTable)
plannedUpkeepsQuery = proc () -> do
  upkeepRow @ (upkeepPK,_,upkeepClosed) <- upkeepQuery -< ()
  (upkeepFK,_,machineFK) <- upkeepMachinesQuery -< ()
  (machinePK,companyFK,_,_,_,_) <- machinesQuery -< ()
  companyRow @ (companyPK,_,_) <- companiesQuery -< ()
  restrict -< (upkeepPK .== upkeepFK)
  restrict -< (machineFK .== machinePK)
  restrict -< (companyPK .== companyFK)
  restrict -< (upkeepClosed .== pgBool False)
  returnA -< (upkeepRow, companyRow)

groupedPlannedUpkeepsQuery :: Query (UpkeepTable, CompaniesTable)
groupedPlannedUpkeepsQuery = 
  AGG.aggregate (p2 (p3(AGG.min, AGG.min, AGG.min), 
    p3(AGG.groupBy, AGG.min, AGG.min))) (plannedUpkeepsQuery)

runCompaniesQuery :: Connection -> IO [(Int, String, String)]
runCompaniesQuery connection = runQuery connection companiesQuery

runMachinesQuery :: Connection -> IO[(Int, Int, Int, Day, Int, Int)]
runMachinesQuery connection = runQuery connection machinesQuery

runMachineTypesQuery :: Connection -> IO[(Int, String, String, Int)]
runMachineTypesQuery connection = runQuery connection machineTypesQuery

runExpandedMachinesQuery' :: Maybe Int -> Connection -> IO[((Int, Int, Int, Day, Int, Int), (Int, String, String, Int))]
runExpandedMachinesQuery' machineId connection =
  runQuery connection (expandedMachinesQuery machineId)

runExpandedMachinesQuery :: Maybe Int -> Connection -> IO[(Int, M.Machine)]
runExpandedMachinesQuery machineId connection = do
  rows <- runExpandedMachinesQuery' machineId connection
  return $ map (\((mId,cId,_,mOs,m3,m4),(_,mtN,mtMf,mtI)) ->
    (mId, M.Machine (MT.MachineType mtN mtMf mtI) cId (dayToYmd mOs) m3 m4)) rows 

runExpandedUpkeepsQuery :: Connection -> IO[((Int, Day, Bool), (Int, String, Int))]
runExpandedUpkeepsQuery connection = runQuery connection expandedUpkeepsQuery

runPlannedUpkeepsQuery :: Connection -> IO[((Int, Day, Bool), (Int, String, String))]
runPlannedUpkeepsQuery connection = runQuery connection groupedPlannedUpkeepsQuery

withConnection :: (Connection -> IO a) -> IO a
withConnection runQ = do
  let connectInfo = defaultConnectInfo {
      connectUser = "haskell"
      , connectDatabase = "crm"
      , connectPassword = "haskell"
      , connectHost = "localhost"
    }
  conn <- connect connectInfo
  result <- runQ conn
  close conn
  return result

type Dependencies = (ReaderT Connection IO :: * -> *)
type IdDependencies = (ReaderT (Connection, Maybe Int) IO :: * -> *)

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

instance FromJSON U.Upkeep where
  parseJSON value = case readFromFay' value of
    Left e -> fail e
    Right ok -> return ok
instance FromJSON UM.UpkeepMachine where
  parseJSON value = case readFromFay' value of
    Left e -> fail e
    Right ok -> return ok

instance JS.JSONSchema U.Upkeep where
  schema = gSchema
instance JS.JSONSchema UM.UpkeepMachine where
  schema = gSchema

instance ToJSON C.Company where
  -- super unsafe
  toJSON c = ((fromJust . showToFay) c)
instance FromJSON C.Company where
  parseJSON value = case (readFromFay' value) of
    Left e -> fail e
    Right ok -> return ok
instance JS.JSONSchema C.Company where
  schema = gSchema
instance ToJSON U.Upkeep where
  toJSON = fromJust . showToFay

instance FromJSON M.Machine where
  parseJSON value = case (readFromFay' value) of
    Left e -> fail e
    Right ok -> return ok

instance ToJSON M.Machine where
  toJSON = fromJust . showToFay
instance JS.JSONSchema M.Machine where
  schema = gSchema
instance JS.JSONSchema D.YearMonthDay where
  schema = gSchema
instance JS.JSONSchema D.Precision where
  schema = gSchema
instance JS.JSONSchema MT.MachineType where
  schema = gSchema

listing :: ListHandler Dependencies
listing = mkListing (jsonO . someO) (const $ do
  rows <- ask >>= \conn -> liftIO $ runCompaniesQuery conn
  return $ map (\(cId, cName, cPlant) -> (cId, C.Company cName cPlant)) rows )

type UrlId = Maybe Int

schema' :: S.Schema Void () Void
schema' = S.withListing () (S.named [])

data UpkeepsListing = UpkeepsAll | UpkeepsPlanned

upkeepSchema :: S.Schema Void UpkeepsListing Void
upkeepSchema = S.withListing UpkeepsAll (S.named [("planned", S.listing UpkeepsPlanned)])

schema'' :: S.Schema UrlId () Void
schema'' = S.withListing () (S.unnamedSingle readMay)

companySchema :: S.Schema UrlId () Void
companySchema = S.withListing () $ S.unnamedSingle readMay

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

prepareReader :: ReaderT (Connection, Maybe Int) IO a
              -> ReaderT (Maybe Int) (ReaderT Connection IO) a
prepareReader reader = let
  outer = ask :: Reader (Maybe Int) (Maybe Int)
  in mapReaderT (\maybeIntIdentity ->
    let
      maybeInt = runIdentity maybeIntIdentity
      inner = ask :: ReaderT Connection IO Connection
      getA = inner >>= (\connection -> let
        aa = runReaderT reader (connection, maybeInt)
        in lift aa)
    in getA) outer

companyResource :: Resource Dependencies IdDependencies UrlId () Void
companyResource = (mkResourceReaderWith (\readerConnId ->
    prepareReader readerConnId
  )) {
  list = const listing
  , create = Just createCompanyHandler
  , name = A.companies
  , schema = companySchema }

machineUpdate :: Handler IdDependencies
machineUpdate = mkInputHandler (jsonI . someI) (\machine -> 
  ask >>= \(conn, id') -> case id' of
    Just (machineId) -> do
      _ <- liftIO $ runMachineUpdate (machineId,machine) conn 
      -- todo singal error if the update didn't hit a row
      return ()
    Nothing -> throwError $ IdentError $ ParseError "provided id is not a number" )

machineSingle :: Handler IdDependencies
machineSingle = mkConstHandler (jsonO . someO) (
  ask >>= (\(conn,id') -> case id' of 
    maybeId @ (Just (_)) -> do
      rows <- liftIO $ runExpandedMachinesQuery maybeId conn
      case rows of
        (_,m) : xs | null xs -> return m
        _ -> throwError $ IdentError $ ParseError "there is no such record with that id"
    Nothing -> throwError $ IdentError $ ParseError "provided id is not a number" ))

machineListing :: ListHandler Dependencies
machineListing = mkListing (jsonO . someO) (const $ 
  ask >>= \conn -> liftIO $ runExpandedMachinesQuery Nothing conn )

upkeepsPlannedListing :: ListHandler Dependencies
upkeepsPlannedListing = mkListing (jsonO . someO) (const $ do
  rows <- ask >>= \conn -> liftIO $ runPlannedUpkeepsQuery conn
  let mappedRows = map (\((_,u2,u3),(_,c2,c3)) -> (U.Upkeep (dayToYmd u2) [] u3, C.Company c2 c3)) rows
  return mappedRows )

upkeepListing :: ListHandler Dependencies
upkeepListing = mkListing (jsonO . someO) (const $ do
  rows <- ask >>= \conn -> liftIO $ runExpandedUpkeepsQuery conn
  return $ foldl (\acc ((upkeepId,date,upkeepClosed),(_,note,machineId)) ->
    let
      addUpkeep' = (upkeepId, U.Upkeep (dayToYmd date) [UM.UpkeepMachine note machineId] upkeepClosed)
      in case acc of
        [] -> [addUpkeep']
        (upkeepId', upkeep) : rest | upkeepId' == upkeepId -> let
          modifiedUpkeep = upkeep {
            U.upkeepMachines = UM.UpkeepMachine note machineId : U.upkeepMachines upkeep }
          in (upkeepId', modifiedUpkeep) : rest
        _ -> addUpkeep' : acc
    ) [] rows )

addUpkeep :: Connection
          -> U.Upkeep
          -> IO Int -- ^ id of the upkeep
addUpkeep connection upkeep = do
  upkeepIds <- runInsertReturning
    connection
    upkeepTable (Nothing, pgDay $ ymdToDay $ U.upkeepDate upkeep, pgBool $ U.upkeepClosed upkeep)
    (\(id',_,_) -> id')
  let
    upkeepId = head upkeepIds -- TODO safe
    insertUpkeepMachine upkeepMachine = do
      _ <- runInsert
        connection
        upkeepMachinesTable (
          pgInt4 upkeepId ,
          pgString $ UM.upkeepMachineNote upkeepMachine ,
          pgInt4 $ UM.upkeepMachineMachineId upkeepMachine )
      return ()
  forM_ (U.upkeepMachines upkeep) insertUpkeepMachine
  return $ head upkeepIds

addMachine :: Connection
           -> M.Machine
           -> IO Int -- ^ id of newly created machine
addMachine connection machine = do
  machineTypeId <- case M.machineType machine of
    MT.MachineTypeId id' -> return $ id'
    MT.MachineType name' manufacturer upkeepPerMileage -> do
      newMachineTypeId <- runInsertReturning
        connection
        machineTypesTable (Nothing, pgString name', pgString manufacturer, pgInt4 upkeepPerMileage)
        (\(id',_,_,_) -> id')
      return $ head newMachineTypeId -- todo safe
  let
    M.Machine _ companyId' machineOperationStartDate' initialMileage mileagePerYear = machine
  machineId <- runInsertReturning
    connection
    machinesTable (Nothing, pgInt4 companyId', pgInt4 machineTypeId, pgDay $ ymdToDay machineOperationStartDate',
      pgInt4 initialMileage, pgInt4 mileagePerYear)
    (\(id',_, _, _,_,_) -> id')
  return $ head machineId -- todo safe

createMachineHandler :: Handler IdDependencies
createMachineHandler = mkInputHandler (jsonO . jsonI . someI . someO) (\newMachine ->
  ask >>= \(connection, maybeInt) -> case maybeInt of
    Just(int) -> liftIO $ addMachine connection (newMachine{M.companyId = int})
    _ -> throwError $ InputError $ ParseError $ "provided id is not a number"
  )

createUpkeepHandler :: Handler IdDependencies
createUpkeepHandler = mkInputHandler (jsonO . jsonI . someI . someO) (\newUpkeep ->
  ask >>= \(connection, maybeInt) -> case maybeInt of
    Just(_) -> liftIO $ addUpkeep connection newUpkeep -- todo check that the machines are belonging to this company
    _ -> throwError $ InputError $ ParseError "provided id is not a number"
  )

companyMachineResource :: Resource IdDependencies IdDependencies Void Void Void
companyMachineResource = mkResourceId {
  name = A.machines
  , schema = S.noListing $ S.named []
  , create = Just createMachineHandler
  }

machineResource :: Resource Dependencies IdDependencies UrlId () Void
machineResource = (mkResourceReaderWith prepareReader) {
  list = const machineListing ,
  get = Just machineSingle ,
  update = Just machineUpdate ,
  name = A.machines ,
  schema = schema'' }

upkeepTopLevelResource :: Resource Dependencies Dependencies Void UpkeepsListing Void
upkeepTopLevelResource = mkResourceId {
  list = \listingType -> case listingType of
    UpkeepsAll -> upkeepListing
    UpkeepsPlanned -> upkeepsPlannedListing ,
  name = A.upkeep ,
  schema = upkeepSchema }

upkeepResource :: Resource IdDependencies IdDependencies Void Void Void
upkeepResource = mkResourceId {
  name = A.upkeep ,
  schema = S.noListing $ S.named [] ,
  create = Just createUpkeepHandler }

router' :: Router Dependencies Dependencies
router' = root `compose` (((route companyResource) `compose` route companyMachineResource)
                                                   `compose` route upkeepResource)
               `compose` route machineResource
               `compose` route upkeepTopLevelResource

api :: Api Dependencies
api = [(mkVersion 1 0 0, Some1 $ router')]
