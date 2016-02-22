{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Crm.Shared.MachineKind where

#ifndef FAY
import GHC.Generics
import Data.Data
import Opaleye      (queryRunnerColumn, PGInt4,
                      QueryRunnerColumnDefault(..), fieldQueryRunnerColumn)
#endif
import Data.Text    (Text, pack)

-- | For the machines, that need to be serviced 1/year, set this repetition and usage
hoursInYear :: Int
hoursInYear = 8760

machineKinds :: [(MachineKindEnum, Text)]
machineKinds = [
  (RotaryScrewCompressor, pack "Šroubový kompresor") ,
  (CondensationDryer, pack "Kondenzační sušička") ,
  (VacuumPump, pack "Vývěva") ,
  (PistonCompressor, pack "Pístový kompresor") ,
  (CoolingUnit, pack "Chladicí jednotka") ,
  (NitrogenGenerator, pack "Generátor dusíku") ,
  (AdsorptionDryer, pack "Adsorpční sušička") ,
  (Filter, pack "Filtr") ,
  (Separator, pack "Odlučovač") ,
  (Other, pack "Jiný") ]

data MachineKindEnum = 
  RotaryScrewCompressor | 
  CondensationDryer | 
  VacuumPump | 
  PistonCompressor | 
  CoolingUnit |
  NitrogenGenerator | 
  AdsorptionDryer |
  Filter |
  Separator |
  Other
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

kindToStringRepr :: MachineKindEnum -> Text
kindToStringRepr = snd . (\i -> machineKinds !! i) . kindToDbRepr

isUpkeepByUs :: MachineKindEnum -> Bool
isUpkeepByUs CoolingUnit = False
isUpkeepByUs AdsorptionDryer = False
isUpkeepByUs CondensationDryer = False
isUpkeepByUs _ = True

data MachineKindSpecific = MachineKindSpecific {
  name :: Text }
#ifdef FAY
  deriving (Eq)
#else
  deriving (Generic, Typeable, Data)
#endif

newMachineKindSpecific :: MachineKindSpecific
newMachineKindSpecific = MachineKindSpecific (pack "")

#ifndef FAY
instance QueryRunnerColumnDefault PGInt4 MachineKindEnum where
  queryRunnerColumnDefault = 
    queryRunnerColumn id dbReprToKind fieldQueryRunnerColumn
#endif
