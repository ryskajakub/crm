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
machineKinds = [
  (RotaryScrewCompressor, "Šroubový kompresor") ,
  (CondensationDryer, "Sušička") ,
  (VacuumPump, "Vývěva") ,
  (PistonCompressor, "Pístový kompresor") ,
  (CoolingUnit, "Chladicí jednotka") ,
  (NitrogenGenerator, "Generátor dusíku") ,
  (AdsorptionDryer, "Adsorpční sušička") ]

data MachineKindEnum = 
  RotaryScrewCompressor | 
  CondensationDryer | 
  VacuumPump | 
  PistonCompressor | 
  CoolingUnit |
  NitrogenGenerator | 
  AdsorptionDryer
#ifdef FAY
  deriving (Eq)
#else
  deriving (Generic, Typeable, Data, Show, Eq)
#endif

kindToDbRepr :: MachineKindEnum -> Int
kindToDbRepr kind = kindToDbRepr' kind (map fst machineKinds)

kindToDbRepr' :: MachineKindEnum -> [MachineKindEnum] -> Int
kindToDbRepr' x' (x:_) | x == x' = 0
kindToDbRepr' x' (_:xs) = 1 + kindToDbRepr' x' xs

dbReprToKind :: Int -> MachineKindEnum
dbReprToKind int = fst $ machineKinds !! int

data MachineKindSpecific = MachineKindSpecific {
  name :: String }
#ifdef FAY
  deriving (Eq)
#else
  deriving (Generic, Typeable, Data, Show)
#endif

newMachineKindSpecific :: MachineKindSpecific
newMachineKindSpecific = MachineKindSpecific ""
