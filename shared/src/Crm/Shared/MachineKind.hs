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

machineKinds :: [(MachineKindEnum, String)]
machineKinds = [(compressorValue, "Kompresor"), (dryerValue, "Sušička")]

type MachineKindData = MachineKind Compressor Dryer

type MachineKindEnum = MachineKind () ()
compressorValue :: MachineKindEnum
compressorValue = CompressorSpecific ()
dryerValue :: MachineKindEnum
dryerValue = DryerSpecific ()

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

kindToDbRepr :: MachineKindEnum -> Int
kindToDbRepr kind = case kind of
  CompressorSpecific _ -> 0
  DryerSpecific _ -> 1

dbReprToKind :: Int -> MachineKindEnum
dbReprToKind int = if int == 0
  then compressorValue
  else dryerValue
