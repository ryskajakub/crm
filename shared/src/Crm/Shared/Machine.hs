{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Crm.Shared.Machine where

#ifndef FAY
import           Control.Lens.TH          (makeLensesFor)
import           GHC.Generics
import           Data.Data
import           Prelude
import           Rest.Info                (Info(..))
#endif
import           Data.Text                (Text, pack)

import           Crm.Shared.YearMonthDay  (YearMonthDay)
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

newtype MachineId' machineId = MachineId { getMachineId :: machineId }
#ifdef FAY
  deriving Eq
#else
  deriving (Eq, Generic, Typeable, Data, Show)
#endif
type MachineId = MachineId' Int
type MachineIdM = MachineId' (Maybe Int)

#ifndef FAY
instance Functor MachineId' where
  f `fmap` (MachineId mId) = MachineId . f $ mId
#endif

data Machine' machineOperationStartDate initialMileage mileagePerYear 
    label serialNumber yearOfManufacture archived note = Machine {
  machineOperationStartDate :: machineOperationStartDate ,
  initialMileage :: initialMileage ,
  mileagePerYear :: mileagePerYear ,
  label_ :: label ,
  serialNumber :: serialNumber ,
  yearOfManufacture :: yearOfManufacture ,
  archived :: archived ,
  note :: note }
#ifndef FAY
  deriving (Generic, Typeable, Data)
makeLensesFor [("machineOperationStartDate", "operationStartDateL")] ''Machine'
#endif

type Machine = Machine' (Maybe YearMonthDay) Int Int Text Text Text Bool Text
type MachineMarkup = Machine' (Maybe YearMonthDay) Int Int Text Text Text Bool [SR.Markup]

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
