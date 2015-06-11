{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}

module Crm.Shared.MachineType where

#ifndef FAY
import GHC.Generics
import Data.Data
import Fay.FFI
import Rest.Info    (Info(..))
#else
import FFI
#endif
import Data.Text    (Text, pack)

import qualified Crm.Shared.UpkeepSequence as US
import qualified Crm.Shared.MachineKind as MK

#ifndef FAY
instance Info MachineTypeId where
  describe _ = "machineTypeId"
#endif

newtype MachineTypeId = MachineTypeId { getMachineTypeId :: Int }
#ifdef FAY
  deriving Eq
#else
  deriving (Generic, Typeable, Data, Show, Read)
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
