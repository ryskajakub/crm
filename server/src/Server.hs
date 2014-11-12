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

import Data.Text (pack, Text)
import Data.JSON.Schema.Generic (gSchema)
import qualified Data.JSON.Schema.Types as JS (JSONSchema(schema))
import Data.Typeable.Internal (Typeable)

import Snap.Http.Server (quickHttpServe)
import Snap.Core (Snap)

import Rest.Api (Api, mkVersion, Some1(Some1), Router, route, root, compose)
import Rest.Driver.Snap (apiToHandler')
import Rest.Resource (Resource, mkResourceId, Void, name, schema, list, create)
import Rest.Schema (Schema, named, withListing)
import Rest.Dictionary.Combinators (jsonO, someO, jsonI, someI)
import Rest.Handler (ListHandler, mkListing, Handler, mkInputHandler)
import Rest.Types.Error (Reason)

import Database.Persist (insert_, delete, deleteWhere, selectList, (==.), SelectOpt(LimitTo), get, Entity)
import Database.Persist.Sql (ConnectionPool)
import Database.Persist.Postgresql (withPostgresqlPool, runMigration, runSqlPersistMPool)
import Database.Persist.TH (mkPersist, mkMigrate, share, sqlSettings, persistLowerCase)

import Generics.Regular
import GHC.Generics

import Debug.Trace(trace)
import Data.Aeson.Types (toJSON)
import Data.Aeson (encode)

type Dependencies = (ReaderT ConnectionPool IO :: * -> *)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Company json
  name String
  plant String
  contact String
  phone String
  address String
  deriving Show Typeable Generic
|]

deriveAll ''Company "PFCompany"
type instance PF Company = PFCompany

instance JS.JSONSchema Company where schema = gSchema

performDb :: (Error a) => (ConnectionPool -> IO()) -> ErrorT a (ReaderT ConnectionPool IO) ()
performDb action = ask >>= \pool -> liftIO $ action pool

insertCompany :: Company -> ConnectionPool -> IO ()
insertCompany company = runSqlPersistMPool $ do
  runMigration migrateAll
  insert_ $ company

listing' :: ListHandler Dependencies
listing' = mkListing (jsonO . someO) (const $ (performDb $ insertCompany $ let
  c = Company "Continental" "I" "p.Jelínek" "721 650 194" "Brandýs nad labem"
  json = encode $ toJSON c
  in trace(show json)(c)
  ) >> return [pack "XXX"])

companySchema :: Schema Company () Void
companySchema = withListing () (named [])

companyCreate :: Handler Dependencies
companyCreate = mkInputHandler (jsonI . someI) (\company -> performDb $ insertCompany company)

companyResource :: Resource Dependencies Dependencies Company () Void
companyResource = mkResourceId {
  list = const listing'
  , name = "company"
  , schema = companySchema
  , create = Just companyCreate
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
