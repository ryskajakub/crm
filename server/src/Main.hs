{-# LANGUAGE OverloadedStrings #-}

module Main where

import Snap.Core (route, method, Snap, Method(POST, GET), putResponse, emptyResponse, readRequestBody, setResponseCode, writeLBS, logError)
import Snap.Http.Server (quickHttpServe)

import Database.MySQL.Simple (defaultConnectInfo, Query, connect, connectDatabase, execute, close, ConnectInfo, insertID, query_, Connection)

import Control.Monad.IO.Class (liftIO)
import Control.Exception (try, SomeException, bracket)

import Data.Aeson(decode, encode, ToJSON, Value, toJSON)
import Data.Word(Word64)
import Data.ByteString.Lazy(toStrict)
import Data.ByteString(append)

import Data.Text(pack, Text)

import Server.Data(IdResponse(IdResponse), name, days, Company(Company))
import Server.Util(hashmapize)

connectionInfo :: ConnectInfo
connectionInfo = defaultConnectInfo { connectDatabase = "crm" }

createCompanyQuery :: Query
createCompanyQuery = "insert into Company(name, days) values (?, ?)"

getAllCompaniesQuery :: Query
getAllCompaniesQuery = "select id, name, days from Company order by name desc"

main :: IO ()
main = quickHttpServe site

executeWithConnection :: (Connection -> IO a) -> IO a
executeWithConnection f =
  bracket (connect connectionInfo) (close) (f)

site :: Snap ()
site =
  route [("/api",
    route [("/companies/new",
      method POST $ do
        requestBody <- readRequestBody 1024
        maybeCompany <- return $ (decode requestBody :: Maybe Company)
        case maybeCompany of
          Just (company) ->
            (=<<) (\x -> x) (liftIO $ bracket
              (connect connectionInfo)
              (close)
              (\connection ->
                let
                  queryResult = (try $ do
                    execute connection createCompanyQuery (name company, days company)
                    insertID connection) :: IO (Either SomeException Word64)
                  response = (fmap (\qr -> case qr of
                    Left _ -> putResponse $ setResponseCode 409 emptyResponse :: Snap()
                    Right recordId ->
                      let
                        encodedId = encode $ IdResponse $ fromIntegral recordId
                      in
                        writeLBS encodedId
                    ) queryResult) :: IO (Snap ())
                in
                  response
              ))
          Nothing -> do
            logError $ ("Failed to parse: ") `append` (toStrict requestBody)
            (putResponse $ setResponseCode 400 emptyResponse)
    ), ("/companies",
      method GET $ (=<<) (\x -> x) $ liftIO $ do
        rows <- executeWithConnection (\connection -> do
          rows <- query_ connection getAllCompaniesQuery
          return $ toJSON $ hashmapize $ fmap (\(companyId, companyName, companyDays) ->
            let 
              key = pack $ show (companyId :: Int) :: Text
              value = toJSON $ Company {name = companyName, days = companyDays} :: Value
            in (key, value) :: (Text, Value)
            ) rows
          )
        return $ writeLBS $ encode rows :: IO (Snap ())
    )]
  )]
