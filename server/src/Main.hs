{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Snap.Core (route, writeBS, method, Snap, Method(GET, POST), putResponse, emptyResponse, readRequestBody)
import Snap.Http.Server (quickHttpServe)
import Database.MySQL.Simple (defaultConnectInfo, Query, query, connect, connectDatabase, execute, close)
import Control.Monad.IO.Class (liftIO)

import Data.Aeson.TH(deriveJSON, defaultOptions)
import Data.Aeson(decode)
import Data.ByteString.Lazy.Char8(unpack)

data Company = Company {
  name :: String
  , days :: Int
} deriving (Show)

$(deriveJSON defaultOptions ''Company)

connectionInfo = defaultConnectInfo { connectDatabase = "crm" }
createCompanyQuery = "insert into Company(name, days) values (?, ?)"

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
  route [("/api",
    route [("/company/new", do
      requestBody <- readRequestBody 1024
      maybeCompany <- return $ (decode requestBody :: Maybe Company)
      case maybeCompany of 
        Just (company) -> liftIO $ do
          connection <- connect connectionInfo
          execute connection createCompanyQuery (name company, days company)
          close connection
        Nothing -> return ()
      method POST $ putResponse emptyResponse
    )]
  )]
