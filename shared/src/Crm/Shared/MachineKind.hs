{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Shared.MachineKind where

#ifndef FAY
import GHC.Generics
import "base" Data.Data
import "base" Prelude
#else
import "fay-base" Prelude
#endif

import Crm.Shared.Dryer
import Crm.Shared.Compressor

machineKinds :: [(Int, String)]
machineKinds = [(0, "Kompresor"), (1, "Sušička")]

type MachineKindData = MachineKind Compressor Dryer
type MachineKindEnum = MachineKind () ()
data MachineKind compressor dryer = 
  CompressorSpecific {
    compressor :: compressor } |
  DryerSpecific {
    dryer :: dryer }
#ifndef FAY
  deriving (Generic, Typeable, Data, Show)
#endif

newCompressorSpecific :: MachineKindData
newCompressorSpecific = CompressorSpecific newCompressor

newDryerSpecific :: MachineKindData
newDryerSpecific = DryerSpecific newDryer
