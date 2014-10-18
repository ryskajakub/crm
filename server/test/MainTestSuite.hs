module Main where

import Test.HUnit(Assertion, assertEqual)
import Test.Framework(defaultMain)
import Test.Framework.Providers.HUnit(testCase)

import Data.Aeson(toJSON, Value(Object), encode)

import Server.Data
import Server.Utils(hashmapize)
import Debug.Trace(trace)

import Data.Text.Encoding(decodeUtf8)
import Data.Text(unpack)
import Data.ByteString.Lazy(toStrict)

sendCompaniesAsHashMap :: Assertion
sendCompaniesAsHashMap = 
  assertEqual "OK" 1 1

main :: IO ()
main = defaultMain [
    testCase "Converts the data from Company table to HashMap, so it has the right format for the frontend" companyToJsonTest
  ]
