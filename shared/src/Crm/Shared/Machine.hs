{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}

module Crm.Shared.Machine where

import Crm.Shared.YearMonthDay (YearMonthDay)

#ifndef FAY
import           GHC.Generics
import           Data.Data
import           Prelude
import           Rest.Info                (Info(..))
#endif
import           Data.Text                (Text, pack)

import qualified Crm.Shared.ContactPerson as CP

#ifndef FAY
instance Info MachineId where
  describe _ = "machineId"
instance Read MachineId where 
  readsPrec i = fmap (\(a,b) -> (MachineId a, b)) `fmap` readsPrec i
#endif

data ContactPersonForMachine = 
  ContactPersonIdForMachine CP.ContactPersonId | 
  ContactPersonForMachine CP.ContactPerson
#ifndef FAY
  deriving (Generic, Typeable, Data)
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
  note :: Text ,
  serialNumber :: Text ,
  yearOfManufacture :: Text }
#ifndef FAY
  deriving (Generic, Typeable, Data)
#endif

newMachine' :: Maybe YearMonthDay -> Machine
newMachine' ymd = Machine {
  machineOperationStartDate = ymd ,
  initialMileage = 0 ,
  note = (pack "") ,
  mileagePerYear = 365 * 24 ,
  serialNumber = (pack "") ,
  yearOfManufacture = (pack "") }

newMachine :: YearMonthDay -> Machine
newMachine ymd = newMachine' $ Just ymd
