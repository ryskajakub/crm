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

machineKinds :: [(MachineKindEnum, String)]
machineKinds = [(Compressor, "Kompresor"), (Dryer, "Sušička")]

data MachineKindEnum = Compressor | Dryer
#ifdef FAY
  deriving (Eq)
#else
  deriving (Generic, Typeable, Data, Show)
#endif

kindToDbRepr :: MachineKindEnum -> Int
kindToDbRepr kind = case kind of
  Compressor -> 0
  Dryer -> 1

dbReprToKind :: Int -> MachineKindEnum
dbReprToKind int = if int == 0
  then Compressor
  else Dryer

data MachineKindSpecific = MachineKindSpecific {
  name :: String }
#ifdef FAY
  deriving (Eq)
#else
  deriving (Generic, Typeable, Data, Show)
#endif
