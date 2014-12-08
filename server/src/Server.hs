{-# LANGUAGE FlexibleContexts #-}

module Server (
  main
) where

import Opaleye.QueryArr (Query)
import Opaleye.Table (Table(Table), required, queryTable)
import Opaleye.Column (Column)

import Data.Profunctor.Product (p2)

import qualified Opaleye.Internal.Unpackspec as U
import Data.Profunctor.Product.Default (Default)
import qualified Opaleye.Sql as Sql
import Database.PostgreSQL.Simple (ConnectInfo(..), Connection, defaultConnectInfo, connect, close, connectPostgreSQL, postgreSQLConnectionString)
import Opaleye.RunQuery (runQuery)

import Control.Monad (forM_)

main :: IO ()
main = (withConnection runCompaniesQuery) >>=
  \list -> (forM_ list (
    \(id, name) -> putStrLn $ "name: " ++ name 
  ))

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
