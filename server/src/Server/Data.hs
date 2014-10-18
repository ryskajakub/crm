{-# LANGUAGE TemplateHaskell #-}

module Server.Data where

import Data.Aeson.TH(deriveJSON, defaultOptions)
import Data.Aeson(decode, encode, ToJSON, Value, toJSON, Object)

import Data.HashMap.Strict(singleton, HashMap, insert)
import Data.Text(pack, Text)

data IdToObject a = IdToObject {
  idValue :: Int
  , object :: a
}

data Company = Company {
  name :: String
  , days :: Int
} deriving (Show)

data IdResponse = IdResponse {
  id :: Int
}

$(deriveJSON defaultOptions ''Company)
$(deriveJSON defaultOptions ''IdResponse)
