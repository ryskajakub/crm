module Main where

import Test.HUnit(Assertion, assertEqual)
import Test.Framework(defaultMain)
import Test.Framework.Providers.HUnit(testCase)

import Data.Aeson(toJSON, Value(Object), encode)

import Server.Data
import Debug.Trace(trace)

import Data.Text.Encoding(decodeUtf8)
import Data.Text(unpack)
import Data.ByteString.Lazy(toStrict)

companyToJsonTest :: Assertion
companyToJsonTest = 
  let
    objectToJSONify = IdToObject {idValue = 5, object = Company {name = "company 1", days = 30} }
    asString = unpack $ decodeUtf8 $ toStrict $ encode objectToJSONify
  in assertEqual
    "Encoding of `IdToObject` failed" 
    "{\"5\":{\"days\":30,\"name\":\"company 1\"}}" 
    asString

main :: IO ()
main = defaultMain [
    testCase "Company to json test" companyToJsonTest
  ]
