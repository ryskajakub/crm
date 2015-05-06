{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Crm.Shared.MachineType where

#ifndef FAY
import GHC.Generics
import "base" Data.Data
import "base" Prelude
import Data.JSON.Schema.Types (JSONSchema)
import Data.Aeson.Types (ToJSON, FromJSON)
#else
import "fay-base" Prelude
#endif

import qualified Crm.Shared.UpkeepSequence as US
import qualified Crm.Shared.MachineKind as MK

newtype MachineTypeId = MachineTypeId { getMachineTypeId :: Int }
#ifdef FAY
  deriving Eq
#else
  deriving (Generic, Typeable, Data, Show, JSONSchema, ToJSON, FromJSON)
#endif

type MachineType' = (MachineTypeId, MachineType)

-- | Machine type can be either an id or the machine type object
data MachineType = MachineType {
  kind :: MK.MachineKindEnum ,
  machineTypeName :: String ,
  machineTypeManufacturer :: String }
#ifndef FAY
  deriving (Generic, Typeable, Data, Show)
#endif

newMachineType :: MachineType
newMachineType = MachineType MK.compressorValue "" ""

data MyEither = 
  MyMachineType (MachineType, [US.UpkeepSequence])
  | MyInt Int
#ifndef FAY
  deriving (Generic, Typeable, Data, Show)
#endif
