{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}

module Server (
  api, main
) where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Logger (runStderrLoggingT, runNoLoggingT, NoLoggingT(NoLoggingT))
import Control.Monad.Error (ErrorT(ErrorT), Error)
import Control.Monad.Reader (ReaderT, ask, mapReaderT, runReaderT)

import Data.Text (pack, Text)

import Snap.Http.Server (quickHttpServe)
import Snap.Core (Snap)

import Rest.Api (Api, mkVersion, Some1(Some1), Router, route, root, compose)
import Rest.Driver.Snap (apiToHandler')
import Rest.Resource (Resource, mkResourceId, Void, name, schema, list)
import Rest.Schema (Schema, named, withListing)
import Rest.Dictionary.Combinators (jsonO, someO)
import Rest.Handler (ListHandler, mkListing)
import Rest.Types.Error (Reason)

import Database.Persist (insert_, delete, deleteWhere, selectList, (==.), SelectOpt(LimitTo), get, Entity)
import Database.Persist.Sql (ConnectionPool)
import Database.Persist.Postgresql (withPostgresqlPool, runMigration, runSqlPersistMPool)
import Database.Persist.TH (mkPersist, mkMigrate, share, sqlSettings, persistLowerCase)

type Dependencies = (ReaderT ConnectionPool IO :: * -> *)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Dog
  name String
  deriving Show
|]

insertDog' :: (Error a) => ErrorT a (ReaderT ConnectionPool IO) ()
insertDog' = ask >>= \pool -> liftIO $ insertDog pool

insertDog :: ConnectionPool -> IO ()
insertDog pool = 
  flip runSqlPersistMPool pool $ do
    runMigration migrateAll
    insert_ $ Dog "Azor"

listing' :: ListHandler Dependencies
listing' = mkListing (jsonO . someO) (const $ insertDog' >> return [pack "XXX"])

dogSchema :: Schema Void () Void
dogSchema = withListing () (named [])

dog' :: Resource Dependencies Dependencies Void () Void
dog' = mkResourceId {
  list = const listing'
  , name = "dogs"
  , schema = dogSchema
  }

router' :: Router Dependencies Dependencies
router' = root `compose` (route dog')

api :: Api Dependencies
api = [(mkVersion 1 0 0, Some1 $ router')]

liftReader :: ConnectionPool -> Dependencies a -> Snap a
liftReader pool deps = liftIO $ runReaderT deps pool

main :: IO ()
main =
  runNoLoggingT $ withPostgresqlPool connStr 10 (\pool -> NoLoggingT $ quickHttpServe $ apiToHandler' (liftReader pool) api)

connStr = "dbname=crm user=coub"
