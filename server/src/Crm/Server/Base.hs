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

import Crm.Shared.Data (Company(Company, companyName))
import Fay.Convert (showToFay)

type CompaniesTable = (Column PGInt4, Column PGText, Column PGText)

companiesTable :: Table CompaniesTable CompaniesTable
companiesTable = Table "companies" (p3 (
    required "id"
    , required "name"
    , required "plant"
  ))

companiesQuery :: Query CompaniesTable
companiesQuery = queryTable companiesTable

runCompaniesQuery :: Connection -> IO [(Int, String, String)]
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
    rows <- ask >>= \conn -> liftIO $ runCompaniesQuery conn
    return $ map (\(cId, cName, plant) -> Company cId cName plant) rows
  )

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
