{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Snap.Core (route, method, Snap, Method(POST), putResponse, emptyResponse, readRequestBody, setResponseCode)
import Snap.Http.Server (quickHttpServe)

import Database.MySQL.Simple (defaultConnectInfo, Query, connect, connectDatabase, execute, close, ConnectInfo)

import Control.Monad.IO.Class (liftIO)
import Control.Exception (try, SomeException, bracket)

import Data.Aeson.TH(deriveJSON, defaultOptions)
import Data.Aeson(decode)

import Data.Int(Int64)

data Company = Company {
  name :: String
  , days :: Int
} deriving (Show)

$(deriveJSON defaultOptions ''Company)

connectionInfo :: ConnectInfo
connectionInfo = defaultConnectInfo { connectDatabase = "crm" }

createCompanyQuery :: Query
createCompanyQuery = "insert into Company(name, days) values (?, ?)"

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
  route [("/api",
    route [("/company/new",
      method POST $ do
        requestBody <- readRequestBody 1024
        maybeCompany <- return $ (decode requestBody :: Maybe Company)
        response <- case maybeCompany of
          Just (company) -> liftIO $ do
            queryResult <- bracket
              (connect connectionInfo)
              (close)
              (\connection -> (try $ (execute connection createCompanyQuery (name company, days company)) :: IO (Either SomeException Int64)))
            return $ case queryResult of
              Left _ -> setResponseCode 409 emptyResponse
              _ -> emptyResponse
          Nothing ->
            return $ setResponseCode 400 emptyResponse
        putResponse response
    )]
  )]
