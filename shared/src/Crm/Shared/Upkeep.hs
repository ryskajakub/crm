{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Shared.Upkeep where

import Crm.Shared.UpkeepMachine
import Crm.Shared.YearMonthDay as D

#ifndef FAY
import GHC.Generics
import "base" Data.Data
import "base" Prelude
#else
import "fay-base" Prelude
#endif

newtype UpkeepId = UpkeepId { getUpkeepId :: Int }

data Upkeep = Upkeep {
  upkeepDate :: D.YearMonthDay ,
  upkeepMachines :: [UpkeepMachine] ,
  upkeepClosed :: Bool }
#ifndef FAY
  deriving (Generic, Typeable, Data, Show)
#endif

newUpkeep :: D.YearMonthDay -> Upkeep
newUpkeep ymd = Upkeep ymd [] False
