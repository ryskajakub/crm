{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Crm.Shared.Data where

import GHC.Generics
import Data.Typeable.Internal (Typeable)

data SCompany = SCompany {
  scName :: String
  , scPlant :: String
} deriving (Generic, Typeable)
