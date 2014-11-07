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
import Control.Monad.Error (ErrorT)

import Data.Text (pack, Text)

import Snap.Http.Server (quickHttpServe)

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

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Dog
  name String
  deriving Show
|]

insertDog :: ConnectionPool -> IO ()
insertDog pool = 
  flip runSqlPersistMPool pool $ do
    runMigration migrateAll
    insert_ $ Dog "Azor"

doSomeIO :: ConnectionPool -> IO [Text]
doSomeIO pool = do
  insertDog pool
  return [pack "ahoj", pack "pse"]

errorTy :: ConnectionPool -> ErrorT (Reason ()) IO [Text]
errorTy pool = liftIO $ doSomeIO pool

listing :: ConnectionPool -> ListHandler IO
listing pool = mkListing (jsonO . someO) (\_ -> errorTy pool)

dogSchema :: Schema Void () Void
dogSchema = withListing () (named [])

dog :: ConnectionPool -> Resource IO IO Void () Void
dog pool = mkResourceId {
    list = \_ -> listing pool
    , name = "dogs"
    , schema = dogSchema
  }

router :: ConnectionPool -> Router IO IO
router pool = root `compose` ( route ( dog pool ))

api :: ConnectionPool -> Api IO
api pool = [(mkVersion 1 0 0, Some1 $ router pool)]

sn :: ConnectionPool -> IO ()
sn pool = quickHttpServe $ (apiToHandler' liftIO $ api pool)

main :: IO ()
main =
  runNoLoggingT $ withPostgresqlPool connStr 10 (\pool -> NoLoggingT $ sn pool)

connStr = "dbname=crm user=coub"
