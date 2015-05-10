{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Shared.Machine where

import Crm.Shared.YearMonthDay (YearMonthDay)

#ifndef FAY
import GHC.Generics
import "base" Data.Data
import "base" Prelude
#else
import "fay-base" Prelude
#endif

newtype MachineId = MachineId { getMachineId :: Int }
#ifdef FAY
  deriving (Eq)
#else
  deriving (Eq, Generic, Typeable, Data, Show)
#endif

data Machine = Machine {
  machineOperationStartDate :: Maybe YearMonthDay ,
  initialMileage :: Int ,
  mileagePerYear :: Int ,
  note :: String ,
  serialNumber :: String ,
  yearOfManufacture :: String }
#ifndef FAY
  deriving (Generic, Typeable, Data, Show)
#endif

newMachine :: YearMonthDay -> Machine
newMachine ymd = Machine {
  machineOperationStartDate = Just ymd ,
  initialMileage = 0 ,
  note = "" ,
  mileagePerYear = 365 * 24 ,
  serialNumber = "" ,
  yearOfManufacture = "" }
