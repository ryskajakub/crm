{-# LANGUAGE PackageImports #-}

module Crm.Data.EmployeeData where

import Crm.Shared.Employee

data EmployeeData = EmployeeData {
  employee :: Employee ,
  employeePageMode :: Maybe EmployeeId }
