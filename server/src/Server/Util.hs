module Server.Util where

import Data.HashMap.Strict(empty, HashMap, insert)
import Data.Aeson(Value)
import Data.Text(pack, Text)

insertIt :: HashMap Text Value -> (Text, Value) -> HashMap Text Value
insertIt acc (key, value) = insert key value acc

hashmapize :: [(Text, Value)] -> HashMap Text Value
hashmapize list = foldl insertIt empty list
