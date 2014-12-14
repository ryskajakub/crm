{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Crm.Server.Base where

import Opaleye.QueryArr (Query)
import Opaleye.Table (Table(Table), required, queryTable)
import Opaleye.Column (Column)

import Data.Profunctor.Product (p3)

import Database.PostgreSQL.Simple (ConnectInfo(..), Connection, defaultConnectInfo, connect, close)
import Opaleye.RunQuery (runQuery)
import Opaleye (PGInt4, PGText)

import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.IO.Class (liftIO)

import Data.JSON.Schema.Generic (gSchema)
import qualified Data.JSON.Schema.Types as JS (JSONSchema(schema))
import Data.Aeson.Types (toJSON, ToJSON)
import Data.Maybe (fromJust)

import Rest.Api (Api, mkVersion, Some1(Some1), Router, route, root, compose)
import Rest.Resource (Resource, mkResourceId, Void, schema, list, name)
import Rest.Schema (Schema, named, withListing)
import Rest.Dictionary.Combinators (jsonO, someO)
import Rest.Handler (ListHandler, mkListing)

import Generics.Regular

import Crm.Shared.Company (Company(Company, companyName))
import Crm.Shared.Machine (Machine(Machine))
import qualified Crm.Shared.Api as A
import Fay.Convert (showToFay)

type CompaniesTable = (Column PGInt4, Column PGText, Column PGText)
type MachinesTable = (Column PGInt4, Column PGInt4, Column PGText)

companiesTable :: Table CompaniesTable CompaniesTable
companiesTable = Table "companies" (p3 (
    required "id"
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

deriveAll ''Company "PFCompany"
type instance PF Company = PFCompany

instance ToJSON Company where
  -- super unsafe
  toJSON c = ((fromJust . showToFay) c)
instance JS.JSONSchema Company where
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
    return $ map (\(cId, cName, cPlant) -> (cId, Company cName cPlant)) rows
  )

schema' :: Schema () () Void
schema' = withListing () (named [])

companyResource :: Resource Dependencies Dependencies () () Void
companyResource = mkResourceId {
  list = const listing
  , name = A.companies
  , schema = schema'
  }

machineListing :: ListHandler Dependencies
machineListing = mkListing (jsonO . someO) (const $ do
    rows <- ask >>= \conn -> liftIO $ runMachinesQuery conn
    return $ map (\(mId, cId, mName) -> (mId, Machine cId mName)) rows
  )

machineResource :: Resource Dependencies Dependencies () () Void
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
