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

-- | Machine type can be either an id or the machine type object
data MachineType =
  MachineTypeId {
    machineTypeId :: Int }
  | MachineType {
    machineTypeName :: String
    , machineTypeManufacturer :: String }
#ifndef FAY
  deriving (Generic, Typeable, Data, Show)
#endif

newMachineType :: MachineType
newMachineType = MachineType "" ""
