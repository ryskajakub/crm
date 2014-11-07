{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Logger (runStderrLoggingT, runNoLoggingT, NoLoggingT(NoLoggingT))

import Data.Text (pack)

import Snap.Http.Server (quickHttpServe)

import Rest.Api (Api, mkVersion, Some1(Some1), Router, route, root, compose)
import Rest.Driver.Snap (apiToHandler')
import Rest.Resource (Resource, mkResourceId, Void, name, schema, list)
import Rest.Schema (Schema, named, withListing)
import Rest.Dictionary.Combinators (jsonO, someO)
import Rest.Handler (ListHandler, mkListing)

import Database.Persist (insert, delete, deleteWhere, selectList, (==.), SelectOpt(LimitTo), get, Entity)
import Database.Persist.Sql (ConnectionPool)
import Database.Persist.Postgresql (withPostgresqlPool, runMigration, runSqlPersistMPool)
import Database.Persist.TH (mkPersist, mkMigrate, share, sqlSettings, persistLowerCase)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
  name String
  deriving Show
|]

listing :: ListHandler IO
listing = mkListing (jsonO . someO) (\_ -> return $ return [pack "ahoj", pack "pse"])

dogSchema :: Schema Void () Void
dogSchema = withListing () (named [])

dog :: Resource IO IO Void () Void
dog = mkResourceId {
    list = \_ -> listing
    , name = "dogs"
    , schema = dogSchema
  }

router :: Router IO IO
router = root `compose` route dog

api :: Api IO
-- api = [(mkVersion 1 0 0, Some1 router)]
api = undefined

sn :: IO ()
sn = quickHttpServe $ (apiToHandler' liftIO api)

main :: IO ()
main =
  runNoLoggingT $ withPostgresqlPool connStr 10 (\pool -> NoLoggingT sn)

connStr = "dbname=crm user=coub"

{-
lifted :: ConnectionPool -> NoLoggingT IO ()
lifted pool = NoLoggingT io
-}

{-
main :: IO ()
main = runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
  flip runSqlPersistMPool pool $ do
    runMigration migrateAll

    johnId <- insert $ Person "John Doe"
    janeId <- insert $ Person "Jane Doe"

    delete janeId
-}
