{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Shared.Data where

#ifndef FAY
import GHC.Generics
import "base" Data.Data
import "base" Prelude
#else
import Prelude
#endif

data Company = Company {
  companyId :: Int
  , companyName :: String
  , companyPlant :: String
} 
#ifndef FAY
  deriving (Generic, Typeable, Data, Show)
#endif
