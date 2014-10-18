module Server.Data where

import Data.Aeson.TH(deriveJSON, defaultOptions)
import Data.Aeson(decode, encode, ToJSON, Value, toJSON, Object)

import Data.HashMap.Strict(singleton, HashMap)
import Data.Text(pack, Text)

data IdToObject a = IdToObject {
  idValue :: Int
  , object :: a
}

instance (ToJSON a) => ToJSON (IdToObject a) where 
  toJSON idToObject =
    let 
      objectAsJson = toJSON $ object idToObject :: Value
      text = (pack $ show $ idValue idToObject) :: Text
      map = singleton text objectAsJson :: HashMap Text Value
      value = toJSON map :: Value
    in
      value
