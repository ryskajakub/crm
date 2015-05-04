{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Shared.MachineKind where

#ifndef FAY
import "base" Prelude
#else
import "fay-base" Prelude
#endif

import Crm.Shared.Dryer
import Crm.Shared.Compressor

machineKinds :: [(Int, String)]
machineKinds = [(0, "Kompresor"), (1, "Sušička")]

data MachineKindSpecific = 
  CompressorSpecific {
    compressor :: Compressor } |
  DryerSpecific {
    dryer :: Dryer }

newCompressorSpecific :: MachineKindSpecific
newCompressorSpecific = CompressorSpecific $ newCompressor

newDryerSpecific :: MachineKindSpecific
newDryerSpecific = DryerSpecific $ newDryer
