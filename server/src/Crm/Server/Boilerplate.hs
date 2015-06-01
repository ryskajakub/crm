{-# OPTIONS -fno-warn-orphans #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Crm.Server.Boilerplate where

import           Generics.Regular          (deriveAll, PF)
import           Data.Aeson.Types          (toJSON, ToJSON, FromJSON, parseJSON, Value)
import qualified Data.JSON.Schema.Types    as JS (JSONSchema(schema))
import           Fay.Convert               (showToFay, readFromFay')
import           Data.Maybe                (fromJust)
import           Data.JSON.Schema.Generic  (gSchema)
import           Data.Data

import qualified Crm.Shared.Company        as C
import qualified Crm.Shared.Employee       as E
import qualified Crm.Shared.ContactPerson  as CP
import qualified Crm.Shared.Machine        as M
import qualified Crm.Shared.MachineType    as MT
import qualified Crm.Shared.MachineKind    as MK
import qualified Crm.Shared.Upkeep         as U
import qualified Crm.Shared.UpkeepSequence as US
import qualified Crm.Shared.UpkeepMachine  as UM
import qualified Crm.Shared.PhotoMeta      as PM
import qualified Crm.Shared.Photo          as P
import qualified Crm.Shared.ExtraField     as EF
import qualified Crm.Shared.Login          as L
import qualified Crm.Shared.YearMonthDay   as D
import           Crm.Shared.MyMaybe

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
instance FromJSON MK.MachineKindEnum where
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
instance ToJSON MK.MachineKindEnum where
  toJSON = fromJust . showToFay
instance (ToJSON a, Data a) => ToJSON (MyMaybe a) where
  toJSON = fromJust . showToFay

instance JS.JSONSchema CP.ContactPerson where
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
instance JS.JSONSchema MK.MachineKindEnum where
  schema = gSchema
instance (JS.JSONSchema a) => JS.JSONSchema (MyMaybe a) where
  schema = gSchema


instance JS.JSONSchema C.CompanyId where
  schema = gSchema
instance ToJSON C.CompanyId where
  toJSON = fromJust . showToFay
instance FromJSON C.CompanyId where
  parseJSON = fayInstance

instance JS.JSONSchema CP.ContactPersonId where
  schema = gSchema
instance ToJSON CP.ContactPersonId where
  toJSON = fromJust . showToFay
instance FromJSON CP.ContactPersonId where
  parseJSON = fayInstance

instance JS.JSONSchema E.EmployeeId where
  schema = gSchema
instance ToJSON E.EmployeeId where
  toJSON = fromJust . showToFay
instance FromJSON E.EmployeeId where
  parseJSON = fayInstance

instance JS.JSONSchema M.MachineId where
  schema = gSchema
instance ToJSON M.MachineId where
  toJSON = fromJust . showToFay
instance FromJSON M.MachineId where
  parseJSON = fayInstance

instance JS.JSONSchema MT.MachineTypeId where
  schema = gSchema
instance ToJSON MT.MachineTypeId where
  toJSON = fromJust . showToFay
instance FromJSON MT.MachineTypeId where
  parseJSON = fayInstance

instance JS.JSONSchema P.PhotoId where
  schema = gSchema
instance ToJSON P.PhotoId where
  toJSON = fromJust . showToFay
instance FromJSON P.PhotoId where
  parseJSON = fayInstance

instance JS.JSONSchema U.UpkeepId where
  schema = gSchema
instance ToJSON U.UpkeepId where
  toJSON = fromJust . showToFay
instance FromJSON U.UpkeepId where
  parseJSON = fayInstance

instance JS.JSONSchema C.Coordinates where
  schema = gSchema
instance ToJSON C.Coordinates where
  toJSON = fromJust . showToFay
instance FromJSON C.Coordinates where
  parseJSON = fayInstance

instance JS.JSONSchema EF.ExtraFieldIdentification where
  schema = gSchema
instance ToJSON EF.ExtraFieldIdentification where
  toJSON = fromJust . showToFay
instance FromJSON EF.ExtraFieldIdentification where
  parseJSON = fayInstance

instance JS.JSONSchema EF.ExtraFieldId where
  schema = gSchema
instance ToJSON EF.ExtraFieldId where
  toJSON = fromJust . showToFay
instance FromJSON EF.ExtraFieldId where
  parseJSON = fayInstance

instance JS.JSONSchema MK.MachineKindSpecific where
  schema = gSchema
instance ToJSON MK.MachineKindSpecific where
  toJSON = fromJust . showToFay
instance FromJSON MK.MachineKindSpecific where
  parseJSON = fayInstance

instance JS.JSONSchema L.Login where
  schema = gSchema
instance ToJSON L.Login where
  toJSON = fromJust . showToFay
instance FromJSON L.Login where
  parseJSON = fayInstance
