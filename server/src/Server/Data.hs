{-# LANGUAGE TemplateHaskell #-}

module Server.Data where

import Data.Aeson.TH(deriveJSON, defaultOptions)
import Data.Aeson(decode, encode, ToJSON, Value, toJSON, Object)

import Data.HashMap.Strict(singleton, HashMap)
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

instance (ToJSON a) => ToJSON (IdToObject a) where 
  toJSON idToObject =
    let 
      objectAsJson = toJSON $ object idToObject :: Value
      text = (pack $ show $ idValue idToObject) :: Text
      map = singleton text objectAsJson :: HashMap Text Value
      value = toJSON map :: Value
    in
      value
