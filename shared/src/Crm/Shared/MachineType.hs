{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Shared.MachineType where

#ifndef FAY
import GHC.Generics
import "base" Data.Data
import "base" Prelude
#else
import "fay-base" Prelude
#endif

import qualified Crm.Shared.UpkeepSequence as US

newtype MachineTypeId = MachineTypeId { getMachineTypeId :: Int }
type MachineType' = (MachineTypeId, MachineType)

-- | Machine type can be either an id or the machine type object
data MachineType = MachineType {
  machineTypeType :: Int ,
  machineTypeName :: String ,
  machineTypeManufacturer :: String }
#ifndef FAY
  deriving (Generic, Typeable, Data, Show)
#endif

newMachineType :: MachineType
newMachineType = MachineType 0 "" ""

data MyEither = 
  MyMachineType (MachineType, [US.UpkeepSequence])
  | MyInt Int
#ifndef FAY
  deriving (Generic, Typeable, Data, Show)
#endif

machineKinds :: [(Int, String)]
machineKinds = [(0, "Kompresor"), (1, "Sušička")]
