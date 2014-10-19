{-# LANGUAGE OverloadedStrings #-}

module Main where

import Snap.Core (route, method, Snap, Method(POST, GET), putResponse, emptyResponse, readRequestBody, setResponseCode, writeLBS, logError, getRequest, rqParam)
import Snap.Http.Server (quickHttpServe)

import Database.MySQL.Simple (defaultConnectInfo, Query, connect, connectDatabase, execute, close, ConnectInfo, insertID, query_, Connection, query)
import Database.MySQL.Simple.Types(Only(Only))

import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (try, SomeException, bracket)

import Data.Aeson(decode, encode, ToJSON, Value, toJSON)
import Data.Word(Word64)
import Data.ByteString.Lazy(toStrict)
import Data.ByteString(append, intercalate)

import Data.Text(pack, Text, unpack)
import Data.Text.Encoding(decodeUtf8)

import Server.Data(IdResponse(IdResponse), name, days, active, Company(Company), FailResponse(FailResponse), OkResponse(OkResponse))
import Server.Util(hashmapize)

connectionInfo :: ConnectInfo
connectionInfo = defaultConnectInfo { connectDatabase = "crm" }

createCompanyQuery :: Query
createCompanyQuery = "insert into Company(name, days, active) values (?, ?, ?)"

getAllCompaniesQuery :: Query
getAllCompaniesQuery = "select id, name, days, active from Company order by name desc"

checkCompanyNameForAvailabilityQuery :: Query
checkCompanyNameForAvailabilityQuery = "select id from Company where name = ?"

main :: IO ()
main = quickHttpServe site

executeWithConnection :: (Connection -> IO a) -> IO a
executeWithConnection f =
  bracket (connect connectionInfo) (close) (f)

site :: Snap ()
site =
  route [("/api",
    route [("/companies/:name/availability", 
      method GET $ do
        request <- getRequest
        let companyNameToBeCheckedMaybe = fmap (unpack . decodeUtf8 . intercalate "") (rqParam "name" request)
        case companyNameToBeCheckedMaybe of
          Just(companyNameToBeChecked) ->
            join $ liftIO $ executeWithConnection (\connection -> do 
              queryResult <- query connection checkCompanyNameForAvailabilityQuery 
                (Only $ companyNameToBeChecked) :: IO ([Only Int])
              let 
                isTheNameTaken = (length queryResult) == 1
                responseBody = if isTheNameTaken
                  then toJSON $ FailResponse companyNameToBeChecked
                  else toJSON $ OkResponse companyNameToBeChecked
              return $ writeLBS $ encode responseBody )
          Nothing -> (putResponse $ setResponseCode 500 emptyResponse)
    ), ("/companies/new",
      method POST $ do
        requestBody <- readRequestBody 1024
        maybeCompany <- return $ (decode requestBody :: Maybe Company)
        case maybeCompany of
          Just (company) ->
            join $ liftIO $ executeWithConnection (\connection ->
              let
                queryResult = (try $ do
                  execute connection createCompanyQuery (name company, days company, active company)
                  insertID connection) :: IO (Either SomeException Word64)
              in (fmap (\qr -> case qr of
                  Left _ -> putResponse $ setResponseCode 409 emptyResponse :: Snap()
                  Right recordId ->
                    let
                      encodedId = encode $ IdResponse $ fromIntegral recordId
                    in
                      writeLBS encodedId
                  ) queryResult) :: IO (Snap ())
            )
          Nothing -> do
            logError $ ("Failed to parse: ") `append` (toStrict requestBody)
            (putResponse $ setResponseCode 400 emptyResponse)
    ), ("/companies",
      method GET $ join $ liftIO $ do
        rows <- executeWithConnection (\connection -> do
          rows <- query_ connection getAllCompaniesQuery
          return $ toJSON $ hashmapize $ fmap (\(companyId, companyName, companyDays, companyActive) ->
            let
              key = pack $ show (companyId :: Int) :: Text
              value = toJSON $ Company {name = companyName, days = companyDays, active = companyActive} :: Value
            in (key, value) :: (Text, Value)
            ) rows
          )
        return $ writeLBS $ encode rows :: IO (Snap ())
    )]
  )]
