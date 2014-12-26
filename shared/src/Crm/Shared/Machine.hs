{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Shared.Machine where

import Crm.Shared.MachineType (MachineType, newMachineType)
import Crm.Shared.YearMonthDay (YearMonthDay(YearMonthDay), Precision(YearPrecision))

#ifndef FAY
import GHC.Generics
import "base" Data.Data
import "base" Prelude
#else
import "fay-base" Prelude
#endif

data Machine = Machine {
  companyId :: Int ,
  machineOperationStartDate :: YearMonthDay ,
  initialMileage :: Int ,
  mileagePerYear :: Int }
#ifndef FAY
  deriving (Generic, Typeable, Data, Show)
#endif

newMachine :: Int -> Machine
newMachine companyId' = Machine {
  companyId = companyId' ,
  machineOperationStartDate = YearMonthDay 1970 1 1 YearPrecision ,
  initialMileage = 0 ,
  mileagePerYear = 365 * 24 }
