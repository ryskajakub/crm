{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Server (
  api, main
) where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Logger (runStderrLoggingT, runNoLoggingT, NoLoggingT(NoLoggingT))
import Control.Monad.Error (ErrorT(ErrorT), Error)
import Control.Monad.Reader (ReaderT, ask, mapReaderT, runReaderT)
import Control.Monad (liftM, forM_)

import Data.Text (pack, Text)
import Data.JSON.Schema.Generic (gSchema)
import qualified Data.JSON.Schema.Types as JS (JSONSchema(schema))
import Data.Typeable.Internal (Typeable)
import Data.Aeson.Types (toJSON, ToJSON, FromJSON, parseJSON)
import Data.Aeson (encode, object, (.=), Value(Null))
import Data.Maybe (fromJust)

import Snap.Http.Server (quickHttpServe)
import Snap.Core (Snap)

import Rest.Api (Api, mkVersion, Some1(Some1), Router, route, root, compose)
import Rest.Driver.Snap (apiToHandler')
import Rest.Resource (Resource, mkResourceId, Void, schema, list, create, statics, name)
import Rest.Schema (Schema, named, withListing, static)
import Rest.Dictionary.Combinators (jsonO, someO, jsonI, someI)
import Rest.Handler (ListHandler, mkListing, Handler, mkInputHandler, mkConstHandler)
import Rest.Types.Error (Reason)

import Database.Persist (insert_, delete, deleteWhere, selectList, (==.), SelectOpt(LimitTo), get, Entity, entityVal, entityKey)
import Database.Persist.Sql (ConnectionPool, fromSqlKey)
import Database.Persist.Postgresql (withPostgresqlPool, runMigration, runSqlPersistMPool)
import Database.Persist.TH (mkPersist, mkMigrate, share, sqlSettings, persistLowerCase)

import Generics.Regular

import Debug.Trace(trace)

import qualified Crm.Shared.Data as D

import Fay.Convert (showToFay)

type Dependencies = (ReaderT ConnectionPool IO :: * -> *)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Company
  name String
  plant String
  contact String
  phone String
  address String
|]

deriveAll ''D.Company "PFCompany"
type instance PF D.Company = PFCompany

instance FromJSON D.Company where
  parseJSON = const $ undefined
instance ToJSON D.Company where
  -- super unsafe
  toJSON c = trace (show c) ((fromJust . showToFay) c)
instance JS.JSONSchema D.Company where
  schema = gSchema

performDb :: (Error a) => (ConnectionPool -> IO b) -> ErrorT a (ReaderT ConnectionPool IO) b
performDb action = ask >>= \pool -> liftIO $ action pool

insertCompany :: Company -> ConnectionPool -> IO ()
insertCompany company = runSqlPersistMPool $ insert_ company

selectAllCompanies :: ConnectionPool -> IO [(Integer, Company)]
selectAllCompanies = runSqlPersistMPool (liftM (map (\e -> let
  v = entityVal e
  k = toInteger $ fromSqlKey $ entityKey e
  in (k, v)
  )) (selectList [] []))

listing :: ListHandler Dependencies
listing = mkListing (jsonO . someO) (const $ do 
    companies <- performDb selectAllCompanies
    let 
      toSharedCompany company = D.Company 
        (fromIntegral $ fst company)
        (companyName $ snd company) 
        (companyPlant $ snd company) 
        (companyContact $ snd company) 
        (companyPhone $ snd company) 
        (companyAddress $ snd company)
      sdCompanies = map toSharedCompany companies
    return sdCompanies
  )

companyCreate :: Handler Dependencies
companyCreate = mkInputHandler (jsonI . someI) (\company -> 
  let company' = company :: D.Company
  in performDb $ insertCompany $ Company "1" "2" "3" "4" "5")

companies :: [Company]
companies = let
  companyBase = Company "Continental" "I" "p.Jelínek" "721 650 194" "Brandýs nad labem"
  companyNames = ["Continental","České dráhy","FOMA Bohemia","Kand","Metrostav","Neumann","PREX","Stachema Kolín","Valsabbia"]
  in map (\name -> companyBase { companyName = name }) companyNames

createCompanyData :: ConnectionPool -> IO ()
createCompanyData = runSqlPersistMPool $ do
  runMigration migrateAll
  forM_ companies (\c -> insert_ c)

createSampleData :: Handler Dependencies
createSampleData = mkConstHandler id (performDb createCompanyData)

companySchema :: Schema () () ()
companySchema = withListing () (named [("migrate", static () )])

companyResource :: Resource Dependencies Dependencies () () ()
companyResource = mkResourceId {
  list = const listing
  , name = "company"
  , schema = companySchema
  , create = Just companyCreate
  , statics = const $ createSampleData
  }

router' :: Router Dependencies Dependencies
router' = root `compose` (route companyResource)

api :: Api Dependencies
api = [(mkVersion 1 0 0, Some1 $ router')]

liftReader :: ConnectionPool -> Dependencies a -> Snap a
liftReader pool deps = liftIO $ runReaderT deps pool

main :: IO ()
main =
  runNoLoggingT $ withPostgresqlPool connStr 10 (\pool -> NoLoggingT $ quickHttpServe $ apiToHandler' (liftReader pool) api)

connStr = "dbname=crm user=coub"
