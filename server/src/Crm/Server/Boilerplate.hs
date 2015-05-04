{-# OPTIONS -fno-warn-orphans #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Crm.Server.Boilerplate where

import Generics.Regular (deriveAll, PF)
import Data.Aeson.Types (toJSON, ToJSON, FromJSON, parseJSON, Value)
import qualified Data.JSON.Schema.Types as JS (JSONSchema(schema))
import Fay.Convert (showToFay, readFromFay')
import Data.Maybe (fromJust)
import Data.JSON.Schema.Generic (gSchema)

import qualified Crm.Shared.Compressor as MC
import qualified Crm.Shared.Dryer as MD
import qualified Crm.Shared.Company as C
import qualified Crm.Shared.ContactPerson as CP
import qualified Crm.Shared.Machine as M
import qualified Crm.Shared.MachineType as MT
import qualified Crm.Shared.MachineKind as MK
import qualified Crm.Shared.Upkeep as U
import qualified Crm.Shared.UpkeepMachine as UM
import qualified Crm.Shared.YearMonthDay as D
import qualified Crm.Shared.Employee as E
import qualified Crm.Shared.UpkeepSequence as US
import qualified Crm.Shared.PhotoMeta as PM
import Crm.Shared.MyMaybe

import Data.Data

deriveAll ''C.Company "PFCompany"
type instance PF C.Company = PFCompany

deriveAll ''M.Machine "PFMachine"
type instance PF M.Machine = PFMachine

deriveAll ''MT.MachineType "PFMachineType"
type instance PF MT.MachineType = PFMachineType

deriveAll ''U.Upkeep "PFUpkeep"
type instance PF U.Upkeep = PFUpkeep

deriveAll ''UM.UpkeepMachine "PFUpkeepMachine"
type instance PF UM.UpkeepMachine = PFUpkeepMachine

fayInstance :: (Monad m, Data a) => Value -> m a
fayInstance value = case readFromFay' value of
  Left e -> fail e
  Right ok -> return ok
instance FromJSON MT.MyEither where
  parseJSON = fayInstance
instance FromJSON MT.MachineType where 
  parseJSON = fayInstance
instance FromJSON C.Company where
  parseJSON = fayInstance
instance FromJSON U.Upkeep where
  parseJSON = fayInstance
instance FromJSON UM.UpkeepMachine where
  parseJSON = fayInstance
instance FromJSON M.Machine where
  parseJSON = fayInstance
instance FromJSON US.UpkeepSequence where
  parseJSON = fayInstance
instance FromJSON PM.PhotoMeta where
  parseJSON = fayInstance
instance FromJSON E.Employee where
  parseJSON = fayInstance
instance FromJSON CP.ContactPerson where
  parseJSON = fayInstance
instance FromJSON MK.MachineKindSpecific where
  parseJSON = fayInstance
instance (FromJSON a, Data a) => FromJSON (MyMaybe a) where
  parseJSON = fayInstance

-- super unsafe
instance ToJSON D.YearMonthDay where
  toJSON = fromJust . showToFay
instance ToJSON C.Company where
  toJSON = fromJust . showToFay
instance ToJSON U.Upkeep where
  toJSON = fromJust . showToFay
instance ToJSON MT.MachineType where
  toJSON = fromJust . showToFay
instance ToJSON M.Machine where
  toJSON = fromJust . showToFay
instance ToJSON UM.UpkeepMachine where
  toJSON = fromJust . showToFay
instance ToJSON E.Employee where
  toJSON = fromJust . showToFay
instance ToJSON US.UpkeepSequence where
  toJSON = fromJust . showToFay
instance ToJSON PM.PhotoMeta where
  toJSON = fromJust . showToFay
instance ToJSON CP.ContactPerson where
  toJSON = fromJust . showToFay
instance (ToJSON a, Data a) => ToJSON (MyMaybe a) where
  toJSON = fromJust . showToFay

instance JS.JSONSchema CP.ContactPerson where
  schema = gSchema
instance JS.JSONSchema MD.Dryer where
  schema = gSchema
instance JS.JSONSchema MC.Compressor where
  schema = gSchema
instance JS.JSONSchema MK.MachineKindSpecific where
  schema = gSchema
instance JS.JSONSchema PM.PhotoMeta where
  schema = gSchema
instance JS.JSONSchema E.Employee where
  schema = gSchema
instance JS.JSONSchema C.Company where
  schema = gSchema
instance JS.JSONSchema M.Machine where
  schema = gSchema
instance JS.JSONSchema D.YearMonthDay where
  schema = gSchema
instance JS.JSONSchema D.Precision where
  schema = gSchema
instance JS.JSONSchema MT.MachineType where
  schema = gSchema
instance JS.JSONSchema Char where
  schema = gSchema
instance JS.JSONSchema MT.MyEither where
  schema = gSchema
instance JS.JSONSchema U.Upkeep where
  schema = gSchema
instance JS.JSONSchema UM.UpkeepMachine where
  schema = gSchema
instance JS.JSONSchema US.UpkeepSequence where
  schema = gSchema
instance (JS.JSONSchema a) => JS.JSONSchema (MyMaybe a) where
  schema = gSchema
