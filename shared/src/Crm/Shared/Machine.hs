{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Shared.Machine where

import Crm.Shared.YearMonthDay (YearMonthDay(YearMonthDay), Precision(YearPrecision))

#ifndef FAY
import GHC.Generics
import "base" Data.Data
import "base" Prelude
#else
import "fay-base" Prelude
#endif

newtype MachineId = MachineId { getMachineId :: Int }

data Machine = Machine {
  machineOperationStartDate :: YearMonthDay ,
  initialMileage :: Int ,
  mileagePerYear :: Int }
#ifndef FAY
  deriving (Generic, Typeable, Data, Show)
#endif

newMachine :: Machine
newMachine = Machine {
  machineOperationStartDate = YearMonthDay 1970 1 1 YearPrecision ,
  initialMileage = 0 ,
  mileagePerYear = 365 * 24 }
