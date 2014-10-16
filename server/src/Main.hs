{-# LANGUAGE OverloadedStrings #-}
module Main where

import Snap.Core (route, writeBS, method, Snap, Method(GET), putResponse, emptyResponse)
import Snap.Http.Server (quickHttpServe)
import Database.MySQL.Simple (defaultConnectInfo, Query, query, connect, connectDatabase, execute)
import Control.Monad.IO.Class (liftIO)

connectionInfo = defaultConnectInfo { connectDatabase = "crm" }
createCompanyQuery = "insert into Company(name, days) values (?, ?)"

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
  route [("/api",
    route [("/company/new", do
      liftIO $ do
        connection <- connect connectionInfo
        execute connection createCompanyQuery ("123" :: String, 345 :: Int)
      method GET $ putResponse emptyResponse
    )]
  )]
