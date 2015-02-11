{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Shared.UpkeepMachine where

import qualified Crm.Shared.Machine as M

#ifndef FAY
import GHC.Generics
import "base" Data.Data
import "base" Prelude
#else
import "fay-base" Prelude
#endif

type UpkeepMachine' = (UpkeepMachine, M.MachineId)

data UpkeepMachine = UpkeepMachine {
  upkeepMachineNote :: String , 
  recordedMileage :: Int ,
  warrantyUpkeep :: Bool }
#ifndef FAY
  deriving (Generic, Typeable, Data, Show)
#endif

newUpkeepMachine :: UpkeepMachine
newUpkeepMachine = UpkeepMachine "" 0 False
