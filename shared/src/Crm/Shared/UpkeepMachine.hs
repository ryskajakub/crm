{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}

module Crm.Shared.UpkeepMachine where

import qualified Crm.Shared.Machine      as M
import qualified Crm.Shared.ServerRender as SR

#ifndef FAY
import GHC.Generics
import Data.Data
#endif
import Data.Text                         (Text, pack)

type UpkeepMachine' = (UpkeepMachine, M.MachineId)

type UpkeepMachine = UpkeepMachineGen Text Text
type UpkeepMachineMarkup = UpkeepMachineGen [SR.Markup] [SR.Markup]

data UpkeepMachineGen upkeepMachineNote endNote = UpkeepMachine {
  upkeepMachineNote :: upkeepMachineNote , 
  recordedMileage :: Int ,
  warrantyUpkeep :: Bool ,
  endNote :: endNote }
#ifndef FAY
  deriving (Generic, Typeable, Data, Show)
#endif

newUpkeepMachine :: UpkeepMachine
newUpkeepMachine = UpkeepMachine (pack "") 0 False (pack "")
