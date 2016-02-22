{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Crm.Shared.Machine where

#ifndef FAY
import           Control.Lens.TH            (makeLensesFor)
import           GHC.Generics
import           Data.Data
import           Prelude
import           Rest.Info                  (Info(..))
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance')
import           Opaleye                    (queryRunnerColumn, PGInt4,
                                              QueryRunnerColumnDefault(..), fieldQueryRunnerColumn)
#endif
import           Data.Text                  (Text, pack)

import           Crm.Shared.YearMonthDay    (YearMonthDay)
import qualified Crm.Shared.ContactPerson   as CP
import qualified Crm.Shared.ServerRender    as SR

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

data Machine' machineOperationStartDate initialMileage mileagePerYear label serialNumber 
    yearOfManufacture archived furtherSpecification upkeepBy = Machine {
  machineOperationStartDate :: machineOperationStartDate ,
  initialMileage :: initialMileage ,
  mileagePerYear :: mileagePerYear ,
  label_ :: label ,
  serialNumber :: serialNumber ,
  yearOfManufacture :: yearOfManufacture ,
  archived :: archived ,
  furtherSpecification :: furtherSpecification ,
  upkeepBy :: upkeepBy }
#ifndef FAY
  deriving (Generic, Typeable, Data)
makeLensesFor [("machineOperationStartDate", "operationStartDateL"),
  ("serialNumber", "serialNumberL")] ''Machine'
#endif

type Machine = Machine' (Maybe YearMonthDay) Int Int Text Text Text Bool Text UpkeepBy
type MachineMarkup = Machine' (Maybe YearMonthDay) Int Int Text Text Text Bool [SR.Markup] UpkeepBy

newMachine' :: Maybe YearMonthDay -> Machine
newMachine' ymd = Machine {
  machineOperationStartDate = ymd ,
  initialMileage = 0 ,
  label_ = pack "" ,
  mileagePerYear = 365 * 24 ,
  serialNumber = pack "" ,
  yearOfManufacture = pack "" ,
  archived = False ,
  furtherSpecification = pack "" ,
  upkeepBy = UpkeepByDefault }

newMachine :: YearMonthDay -> Machine
newMachine ymd = newMachine' $ Just ymd

data UpkeepBy = UpkeepByDefault | UpkeepByWe | UpkeepByThem
#ifndef FAY
  deriving (Generic, Typeable, Data, Show, Eq)
#endif

upkeepByDecode :: Int -> UpkeepBy
upkeepByDecode 1 = UpkeepByWe
upkeepByDecode (-1) = UpkeepByThem
upkeepByDecode _ = UpkeepByDefault

upkeepByEncode :: UpkeepBy -> Int
upkeepByEncode UpkeepByDefault = 0
upkeepByEncode UpkeepByWe = 1
upkeepByEncode UpkeepByThem = (-1)

#ifndef FAY
makeAdaptorAndInstance' ''MachineId'
makeAdaptorAndInstance' ''Machine'

instance QueryRunnerColumnDefault PGInt4 UpkeepBy where
  queryRunnerColumnDefault = 
    queryRunnerColumn id upkeepByDecode fieldQueryRunnerColumn
#endif
