{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Shared.Upkeep where

import Crm.Shared.YearMonthDay as D
import Crm.Shared.UpkeepMachine as UM

#ifndef FAY
import GHC.Generics
import "base" Data.Data
import "base" Prelude
#else
import "fay-base" Prelude
#endif

newtype UpkeepId = UpkeepId { getUpkeepId :: Int }

type Upkeep'' = (UpkeepId, Upkeep)
type Upkeep' = (UpkeepId, Upkeep, [UM.UpkeepMachine'])

data Upkeep = Upkeep {
  upkeepDate :: D.YearMonthDay ,
  upkeepClosed :: Bool ,
  workHours :: String ,
  workDescription :: String ,
  recommendation :: String }
#ifndef FAY
  deriving (Generic, Typeable, Data, Show)
#endif

newUpkeep :: D.YearMonthDay -> Upkeep
newUpkeep ymd = Upkeep ymd False "0" "" ""
