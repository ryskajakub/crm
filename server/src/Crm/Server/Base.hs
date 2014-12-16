{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PackageImports #-}

module Crm.Server.Base where

import Opaleye.QueryArr (Query)
import Opaleye.Table (Table(Table), required, queryTable, optional)
import Opaleye.Column (Column)

import Data.Profunctor.Product (p3, p4)

import Database.PostgreSQL.Simple (ConnectInfo(..), Connection, defaultConnectInfo, connect, close)
import Opaleye.RunQuery (runQuery)
import Opaleye (PGInt4, PGText, pgString, runInsertReturning)
import Opaleye.PGTypes (pgInt4)

import "mtl" Control.Monad.Reader (Reader, ReaderT, ask, withReaderT, runReaderT, mapReaderT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans.Class (lift)

import Data.JSON.Schema.Generic (gSchema)
import qualified Data.JSON.Schema.Types as JS (JSONSchema(schema))
import Data.Aeson.Types (toJSON, ToJSON, FromJSON, parseJSON)
import Data.Maybe (fromJust)
import Data.Functor.Identity (runIdentity)

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
import Fay.Convert (showToFay, readFromFay')

import Safe (readMay)

type CompaniesTable = (Column PGInt4, Column PGText, Column PGText)
type CompaniesWriteTable = (Maybe (Column PGInt4), Column PGText, Column PGText)

type MachinesTable = (Column PGInt4, Column PGInt4, Column PGInt4, Column PGText)
type MachinesWriteTable = (Maybe (Column PGInt4), Column PGInt4, Column PGInt4, Column PGText)

type MachineTypesTable = (Column PGInt4, Column PGText, Column PGText)
type MachineTypesWriteTable = (Maybe (Column PGInt4), Column PGText, Column PGText)

companiesTable :: Table CompaniesWriteTable CompaniesTable
companiesTable = Table "companies" (p3 (
    optional "id"
    , required "name"
    , required "plant"
  ))

machinesTable :: Table MachinesWriteTable MachinesTable
machinesTable = Table "machines" (p4 (
    optional "id"
    , required "company_id"
    , required "machine_type_id"
    , required "operation_start"
  ))

machineTypesTable :: Table MachineTypesWriteTable  MachineTypesTable
machineTypesTable = Table "machine_types" (p3 (
    optional "id"
    , required "name"
    , required "manufacturer"
  ))

companiesQuery :: Query CompaniesTable
companiesQuery = queryTable companiesTable

machinesQuery :: Query MachinesTable
machinesQuery = queryTable machinesTable

machineTypesQuery :: Query MachineTypesTable
machineTypesQuery = queryTable machineTypesTable

runCompaniesQuery :: Connection -> IO [(Int, String, String)]
runCompaniesQuery connection = runQuery connection companiesQuery

runMachinesQuery :: Connection -> IO[(Int, Int, Int, String)]
runMachinesQuery connection = runQuery connection machinesQuery

runMachineTypesQuery :: Connection -> IO[(Int, String, String)]
runMachineTypesQuery connection = runQuery connection machineTypesQuery

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

instance ToJSON C.Company where
  -- super unsafe
  toJSON c = ((fromJust . showToFay) c)
instance FromJSON C.Company where
  parseJSON value = case (readFromFay' value) of
    Left e -> fail e
    Right ok -> return ok
instance JS.JSONSchema C.Company where
  schema = gSchema

instance FromJSON M.Machine where
  parseJSON value = case (readFromFay' value) of
    Left e -> fail e
    Right ok -> return ok

instance ToJSON M.Machine where
  toJSON = fromJust . showToFay
instance JS.JSONSchema M.Machine where
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
  rows <- ask >>= \conn -> liftIO $ runMachinesQuery conn
  return $ map (\(mId, cId, mtId, operationStart) ->
    (mId, M.Machine (MT.MachineTypeId mtId) cId operationStart)) rows
  )

addMachine :: Connection
           -> M.Machine
           -> IO Int -- ^ id of newly created machine
addMachine connection machine = do
  machineTypeId <- case M.machineType machine of
    MT.MachineTypeId id' -> return $ id'
    MT.MachineType name' manufacturer -> do
      newMachineTypeId <- runInsertReturning
        connection
        machineTypesTable (Nothing, pgString name', pgString manufacturer)
        (\(id', _, _) -> id')
      return $ head newMachineTypeId -- todo safe
  let M.Machine _ companyId' machineOperationStartDate' = machine
  machineId <- runInsertReturning
    connection
    machinesTable (Nothing, pgInt4 companyId', pgInt4 machineTypeId, pgString machineOperationStartDate')
    (\(id', _, _, _) -> id')
  return $ head machineId -- todo safe

createMachineHandler :: Handler IdDependencies
createMachineHandler = mkInputHandler (jsonO . jsonI . someI . someO) (\newMachine ->
  ask >>= \(connection, maybeInt) -> case maybeInt of
    Just(int) -> liftIO $ addMachine connection (newMachine{M.companyId = int})
    _ -> throwError $ InputError $ ParseError $ "provided id is not a number"
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

router' :: Router Dependencies Dependencies
router' = root `compose` ((route companyResource) `compose` (route companyMachineResource))
               `compose` (route machineResource)

api :: Api Dependencies
api = [(mkVersion 1 0 0, Some1 $ router')]
