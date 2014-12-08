{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Crm.Server.Base where

import Opaleye.QueryArr (Query)
import Opaleye.Table (Table(Table), required, queryTable)
import Opaleye.Column (Column)

import Data.Profunctor.Product (p2)

import Database.PostgreSQL.Simple (ConnectInfo(..), Connection, defaultConnectInfo, connect, close)
import Opaleye.RunQuery (runQuery)

import Control.Monad.Reader (ReaderT)

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

import Crm.Shared.Data (Company(Company, companyName))
import Fay.Convert (showToFay)

companiesTable :: Table (Column Int, Column String)
                        (Column Int, Column String)
companiesTable = Table "companies" (p2 (
    required "id"
    , required "name"
  ))

companiesQuery :: Query (Column Int, Column String)
companiesQuery = queryTable companiesTable

runCompaniesQuery :: Connection -> IO [(Int, String)]
runCompaniesQuery connection = runQuery connection companiesQuery

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

listing :: ListHandler Dependencies
listing = mkListing (jsonO . someO) (const $ do 
    return companies
  )

companies :: [Company]
companies = let
  companyBase = Company 0 "Continental" "I" "p.Jelínek" "721 650 194" "Brandýs nad labem"
  companyNames = ["Continental","České dráhy","FOMA Bohemia","Kand","Metrostav","Neumann","PREX","Stachema Kolín","Valsabbia"]
  in map (\name' -> companyBase { companyName = name' }) companyNames

companySchema :: Schema () () Void
companySchema = withListing () (named [])

companyResource :: Resource Dependencies Dependencies () () Void
companyResource = mkResourceId {
  list = const listing
  , name = "company"
  , schema = companySchema
  }

router' :: Router Dependencies Dependencies
router' = root `compose` (route companyResource)

api :: Api Dependencies
api = [(mkVersion 1 0 0, Some1 $ router')]
