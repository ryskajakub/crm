{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}

module Crm.Shared.Data where

#ifndef FAY
import GHC.Generics
import Data.Data
#endif

data Company = Company {
  companyId :: Int
  , companyName :: String
  , plant :: String
  , contact :: String
  , phone :: String
  , address :: String
} 
#ifndef FAY
  deriving (Generic, Typeable, Data, Show)
#endif
