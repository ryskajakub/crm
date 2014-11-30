{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Crm.Shared.Data where

import GHC.Generics
import Data.Data

data Company = Company {
  id :: Int
  , name :: String
  , plant :: String
  , contact :: String
  , phone :: String
  , address :: String
} deriving (Generic, Typeable, Data, Show)
