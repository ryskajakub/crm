{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}

module Crm.Shared.MachineType where

#ifndef FAY
import GHC.Generics
import Data.Data
import Fay.FFI
#else
import FFI
#endif
import Data.Text (Text, pack)

import qualified Crm.Shared.UpkeepSequence as US
import qualified Crm.Shared.MachineKind as MK

newtype MachineTypeId = MachineTypeId { getMachineTypeId :: Int }
#ifdef FAY
  deriving Eq
#else
  deriving (Generic, Typeable, Data, Show)
#endif

type MachineType' = (MachineTypeId, MachineType)

-- | Machine type can be either an id or the machine type object
data MachineType = MachineType {
  kind :: Automatic MK.MachineKindEnum ,
  machineTypeName :: Text ,
  machineTypeManufacturer :: Text }
#ifndef FAY
  deriving (Generic, Typeable, Data)
#endif

newMachineType :: MachineType
newMachineType = MachineType MK.RotaryScrewCompressor (pack "") (pack "")

data MyEither = 
  MyMachineType (MachineType, [US.UpkeepSequence])
  | MyInt Int
#ifndef FAY
  deriving (Generic, Typeable, Data)
#endif
