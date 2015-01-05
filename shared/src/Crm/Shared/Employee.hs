{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Shared.Employee where

#ifndef FAY
import GHC.Generics
import "base" Data.Data
import "base" Prelude
#else
import "fay-base" Prelude
#endif

newtype EmployeeId = EmployeeId { getEmployeeId :: Int }
  deriving Eq

type Employee' = (EmployeeId, Employee)

data Employee = Employee {
  name :: String }
#ifndef FAY
  deriving (Generic, Typeable, Data, Show)
#endif
