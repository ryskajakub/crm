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
import qualified Crm.Shared.ServerRender  as SR

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

data Machine' note = Machine {
  machineOperationStartDate :: Maybe YearMonthDay ,
  initialMileage :: Int ,
  mileagePerYear :: Int ,
  label_ :: Text ,
  serialNumber :: Text ,
  yearOfManufacture :: Text ,
  archived :: Bool ,
  note :: note }
#ifndef FAY
  deriving (Generic, Typeable, Data)
#endif

type Machine = Machine' Text
type MachineMarkup = Machine' [SR.Markup]

newMachine' :: Maybe YearMonthDay -> Machine
newMachine' ymd = Machine {
  machineOperationStartDate = ymd ,
  initialMileage = 0 ,
  label_ = (pack "") ,
  mileagePerYear = 365 * 24 ,
  serialNumber = (pack "") ,
  yearOfManufacture = (pack "") ,
  archived = False ,
  note = pack "" }

newMachine :: YearMonthDay -> Machine
newMachine ymd = newMachine' $ Just ymd
