{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Shared.Upkeep where

import Crm.Shared.UpkeepMachine

#ifndef FAY
import GHC.Generics
import "base" Data.Data
import "base" Prelude
#else
import "fay-base" Prelude
#endif

data Upkeep = Upkeep {
  upkeepDate :: String
  , upkeepMachines :: [UpkeepMachine]}
#ifndef FAY
  deriving (Generic, Typeable, Data, Show)
#endif

newUpkeep :: Upkeep
newUpkeep = Upkeep "" []
