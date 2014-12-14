{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Crm.Server.Base where

import Opaleye.QueryArr (Query)
import Opaleye.Table (Table(Table), required, queryTable, optional)
import Opaleye.Column (Column)

import Data.Profunctor.Product (p3)

import Database.PostgreSQL.Simple (ConnectInfo(..), Connection, defaultConnectInfo, connect, close)
import Opaleye.RunQuery (runQuery)
import Opaleye (PGInt4, PGText, arrangeInsertSql, runInsert, pgString)

import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.IO.Class (liftIO)

import Data.JSON.Schema.Generic (gSchema)
import qualified Data.JSON.Schema.Types as JS (JSONSchema(schema))
import Data.Aeson.Types (toJSON, ToJSON, FromJSON, parseJSON)
import Data.Maybe (fromJust)

import Rest.Api (Api, mkVersion, Some1(Some1), Router, route, root, compose)
import Rest.Resource (Resource, mkResourceId, Void, schema, list, name, create)
import Rest.Schema (Schema, named, withListing)
import Rest.Dictionary.Combinators (jsonO, someO, jsonI, someI)
import Rest.Handler (ListHandler, mkListing, mkInputHandler, Handler)

import Generics.Regular

import qualified Crm.Shared.Company as C
import Crm.Shared.Machine (Machine(Machine))
import qualified Crm.Shared.Api as A
import Fay.Convert (showToFay, readFromFay')

type CompaniesTable = (Column PGInt4, Column PGText, Column PGText)
type CompaniesWriteTable = (Maybe (Column PGInt4), Column PGText, Column PGText)

type MachinesTable = (Column PGInt4, Column PGInt4, Column PGText)

companiesTable :: Table CompaniesWriteTable CompaniesTable
companiesTable = Table "companies" (p3 (
    optional "id"
    , required "name"
    , required "plant"
  ))

machinesTable :: Table MachinesTable MachinesTable
machinesTable = Table "machines" (p3 (
    required "id"
    , required "company_id"
    , required "name"
  ))

companiesQuery :: Query CompaniesTable
companiesQuery = queryTable companiesTable

machinesQuery :: Query MachinesTable
machinesQuery = queryTable machinesTable

runCompaniesQuery :: Connection -> IO [(Int, String, String)]
runCompaniesQuery connection = runQuery connection companiesQuery

runMachinesQuery :: Connection -> IO[(Int, Int, String)]
runMachinesQuery connection = runQuery connection machinesQuery

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

deriveAll ''C.Company "PFCompany"
type instance PF C.Company = PFCompany

instance ToJSON C.Company where
  -- super unsafe
  toJSON c = ((fromJust . showToFay) c)
instance FromJSON C.Company where
  parseJSON value = case (readFromFay' value) of
    Left error -> fail error
    Right ok -> return ok
instance JS.JSONSchema C.Company where
  schema = gSchema

deriveAll ''Machine "PFMachine"
type instance PF Machine = PFMachine

instance ToJSON Machine where
  toJSON = fromJust . showToFay
instance JS.JSONSchema Machine where
  schema = gSchema

listing :: ListHandler Dependencies
listing = mkListing (jsonO . someO) (const $ do
  rows <- ask >>= \conn -> liftIO $ runCompaniesQuery conn
  return $ map (\(cId, cName, cPlant) -> (cId, C.Company cName cPlant)) rows
  )

schema' :: Schema Void () Void
schema' = withListing () (named [])

addCompany :: Connection -- ^ database connection
           -> C.Company -- ^ company to save in the db
           -> IO ()
addCompany connection newCompany = do
  runInsert connection
    companiesTable (Nothing, pgString $ C.companyName newCompany, pgString $ C.companyPlant newCompany)
  return ()

createCompanyHandler :: Handler Dependencies
createCompanyHandler = mkInputHandler (jsonI . someI) (\newCompany ->
  ask >>= \conn -> liftIO $ addCompany conn newCompany)

companyResource :: Resource Dependencies Dependencies Void () Void
companyResource = mkResourceId {
  list = const listing
  , create = Just createCompanyHandler
  , name = A.companies
  , schema = schema'
  }

machineListing :: ListHandler Dependencies
machineListing = mkListing (jsonO . someO) (const $ do
  rows <- ask >>= \conn -> liftIO $ runMachinesQuery conn
  return $ map (\(mId, cId, mName) -> (mId, Machine cId mName)) rows
  )

machineResource :: Resource Dependencies Dependencies Void () Void
machineResource = mkResourceId {
  list = const machineListing
  , name = A.machines
  , schema = schema'
  }

router' :: Router Dependencies Dependencies
router' = root `compose` (route companyResource)
               `compose` (route machineResource)

api :: Api Dependencies
api = [(mkVersion 1 0 0, Some1 $ router')]
