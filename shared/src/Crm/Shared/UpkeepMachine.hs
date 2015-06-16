{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}

module Crm.Shared.UpkeepMachine where

import qualified Crm.Shared.Machine as M

#ifndef FAY
import GHC.Generics
import Data.Data
#endif
import Data.Text                    (Text, pack)

type UpkeepMachine' = (UpkeepMachine, M.MachineId)

data UpkeepMachine = UpkeepMachine {
  upkeepMachineNote :: Text , 
  recordedMileage :: Int ,
  warrantyUpkeep :: Bool ,
  endNote :: Text }
#ifndef FAY
  deriving (Generic, Typeable, Data)
#endif

newUpkeepMachine :: UpkeepMachine
newUpkeepMachine = UpkeepMachine (pack "") 0 False (pack "")
