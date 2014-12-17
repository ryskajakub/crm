{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Shared.UpkeepMachine where

#ifndef FAY
import GHC.Generics
import "base" Data.Data
import "base" Prelude
#else
import "fay-base" Prelude
#endif

data UpkeepMachine = UpkeepMachine {
  upkeepMachineNote :: String
  , upkeepMachineMachineId :: Int }
#ifndef FAY
  deriving (Generic, Typeable, Data, Show)
#endif
