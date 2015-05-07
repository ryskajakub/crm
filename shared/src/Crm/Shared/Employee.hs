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
#ifdef FAY
  deriving Eq
#else
  deriving (Generic, Typeable, Data, Show)
#endif

type Employee' = (EmployeeId, Employee)

data Employee = Employee {
  name :: String ,
  contact :: String ,
  capabilities :: String }
#ifndef FAY
  deriving (Generic, Typeable, Data, Show)
#endif

newEmployee :: Employee
newEmployee = Employee "" "" ""
