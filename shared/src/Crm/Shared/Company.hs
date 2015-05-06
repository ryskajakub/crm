{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Shared.Company where

#ifndef FAY
import GHC.Generics
import "base" Data.Data
import "base" Prelude
#else
import "fay-base" Prelude
#endif

newtype CompanyId = CompanyId { getCompanyId :: Int }
#ifndef FAY
  deriving (Generic, Typeable, Data, Show)
#endif

data OrderType = CompanyName | NextService
  deriving (Show, Read)

data Company = Company {
  companyName :: String , 
  companyPlant :: String ,
  companyAddress :: String }
#ifndef FAY
  deriving (Generic, Typeable, Data, Show)
#endif

newCompany :: Company
newCompany = Company "" "" ""
