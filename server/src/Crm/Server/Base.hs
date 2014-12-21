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
import Opaleye (PGInt4, PGText, pgString, runInsertReturning)
import Opaleye.Operators ((.==), restrict)
import Opaleye.PGTypes (pgInt4, PGDate, pgDay)
import Opaleye.Manipulation (runInsert)

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
import Data.Time.Calendar (fromGregorian, Day, toGregorian)

import Rest.Api (Api, mkVersion, Some1(Some1), Router, route, root, compose)
import Rest.Resource (Resource, mkResourceId, Void, schema, list, name, create, mkResourceReaderWith)
import Rest.Schema (Schema, named, withListing, unnamedSingle, noListing)
import Rest.Dictionary.Combinators (jsonO, someO, jsonI, someI, someE, jsonE)
import Rest.Handler (ListHandler, mkListing, mkInputHandler, Handler)
import Rest.Types.Error (DataError(ParseError), Reason(InputError))

import Generics.Regular

import qualified Crm.Shared.Company as C
import qualified Crm.Shared.Machine as M
import qualified Crm.Shared.MachineType as MT
import qualified Crm.Shared.Api as A
import qualified Crm.Shared.Upkeep as U
import qualified Crm.Shared.UpkeepMachine as UM
import qualified Crm.Shared.YearMonthDay as D
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

type UpkeepTable = (DBInt, DBDate)
type UpkeepWriteTable = (Maybe DBInt, DBDate)

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
upkeepTable = Table "upkeeps" $ p2 (
  optional "id" ,
  required "date_" )

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

-- | query, that returns expanded machine type, not just the id
expandedMachinesQuery :: Query (MachinesTable, MachineTypesTable)
expandedMachinesQuery = proc () -> do
  machineRow @ (_,_,machineTypeId,_,_,_) <- machinesQuery -< ()
  machineTypesRow @ (machineTypeId',_,_,_) <- machineTypesQuery -< ()
  restrict -< machineTypeId' .== machineTypeId
  returnA -< (machineRow, machineTypesRow)

expandedUpkeepsQuery :: Query (UpkeepTable, UpkeepMachinesTable)
expandedUpkeepsQuery = proc () -> do
  upkeepRow @ (upkeepId,_) <- upkeepQuery -< ()
  upkeepMachineRow @ (upkeepId',_,_) <- upkeepMachinesQuery -< ()
  restrict -< (upkeepId' .== upkeepId)
  returnA -< (upkeepRow, upkeepMachineRow)

runCompaniesQuery :: Connection -> IO [(Int, String, String)]
runCompaniesQuery connection = runQuery connection companiesQuery

runMachinesQuery :: Connection -> IO[(Int, Int, Int, Day, Int, Int)]
runMachinesQuery connection = runQuery connection machinesQuery

runMachineTypesQuery :: Connection -> IO[(Int, String, String, Int)]
runMachineTypesQuery connection = runQuery connection machineTypesQuery

runExpandedMachinesQuery :: Connection -> IO[((Int, Int, Int, Day, Int, Int), (Int, String, String, Int))]
runExpandedMachinesQuery connection = runQuery connection expandedMachinesQuery

runExpandedUpkeepsQuery :: Connection -> IO[((Int, Day), (Int, String, Int))]
runExpandedUpkeepsQuery connection = runQuery connection expandedUpkeepsQuery

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
  return $ map (\(cId, cName, cPlant) -> (cId, C.Company cName cPlant)) rows
  )

type UrlId = Maybe Int

schema' :: Schema Void () Void
schema' = withListing () (named [])

companySchema :: Schema UrlId () Void
companySchema = withListing () $ unnamedSingle readMay

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

machineListing :: ListHandler Dependencies
machineListing = mkListing (jsonO . someO) (const $ do
  rows <- ask >>= \conn -> liftIO $ runExpandedMachinesQuery conn
  return $ map (\((mId,cId,_,mOs,m3,m4),(_,mtN,mtMf,mtI)) ->
    let (y,m,d) = toGregorian mOs
    in (mId, M.Machine (MT.MachineType mtN mtMf mtI) cId (D.YearMonthDay (fromIntegral y) m d D.DayPrecision) m3 m4)) rows )

upkeepListing :: ListHandler Dependencies
upkeepListing = mkListing (jsonO . someO) (const $ do
  rows <- ask >>= \conn -> liftIO $ runExpandedUpkeepsQuery conn
  return $ foldl (\acc ((upkeepId,date),(_,note,machineId)) ->
    let
      (y,m,d) = toGregorian date
      ymd = D.YearMonthDay (fromIntegral y) m d D.DayPrecision
      addUpkeep' = (upkeepId, U.Upkeep ymd [UM.UpkeepMachine note machineId])
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
  let 
    D.YearMonthDay year month day' _  = U.upkeepDate upkeep
    day = fromGregorian (toInteger year) month day'
  upkeepIds <- runInsertReturning
    connection
    upkeepTable (Nothing, pgDay day)
    (\(id',_) -> id')
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
    D.YearMonthDay year month day' _ = machineOperationStartDate'
    day = fromGregorian (toInteger year) month day'
  machineId <- runInsertReturning
    connection
    machinesTable (Nothing, pgInt4 companyId', pgInt4 machineTypeId, pgDay day,
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
  , schema = noListing $ named []
  , create = Just createMachineHandler
  }

machineResource :: Resource Dependencies Dependencies Void () Void
machineResource = mkResourceId {
  list = const machineListing
  , name = A.machines
  , schema = schema'
  }

upkeepTopLevelResource :: Resource Dependencies Dependencies Void () Void
upkeepTopLevelResource = mkResourceId {
  list = const upkeepListing ,
  name = A.upkeep ,
  schema = schema' }

upkeepResource :: Resource IdDependencies IdDependencies Void Void Void
upkeepResource = mkResourceId {
  name = A.upkeep ,
  schema = noListing $ named [] ,
  create = Just createUpkeepHandler }

router' :: Router Dependencies Dependencies
router' = root `compose` (((route companyResource) `compose` (route companyMachineResource))
                                                   `compose` (route upkeepResource))
               `compose` route machineResource
               `compose` route upkeepTopLevelResource

api :: Api Dependencies
api = [(mkVersion 1 0 0, Some1 $ router')]
