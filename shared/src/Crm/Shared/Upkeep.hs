{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Crm.Shared.Upkeep where

import Crm.Shared.YearMonthDay as D
import Crm.Shared.UpkeepMachine as UM

#ifndef FAY
import GHC.Generics
import "base" Data.Data
import "base" Prelude
import Data.JSON.Schema.Types (JSONSchema)
import Data.Aeson.Types (ToJSON, FromJSON)
#else
import "fay-base" Prelude
#endif

newtype UpkeepId = UpkeepId { getUpkeepId :: Int }
#ifdef FAY
  deriving Eq
#else
  deriving (Generic, Typeable, Data, Show, JSONSchema, ToJSON, FromJSON)
#endif

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
