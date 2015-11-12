{-# OPTIONS -fno-warn-orphans #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Crm.Server.Boilerplate where

import Crm.TH                              (mkFayTransferables, fayInstance)

import           Data.Aeson.Types          (toJSON, ToJSON, FromJSON, parseJSON)
import qualified Data.JSON.Schema.Types    as JS (JSONSchema(schema))
import           Fay.Convert               (showToFay)
import           Data.Maybe                (fromJust)
import           Data.JSON.Schema.Generic  (gSchema)
import           Data.Data

import qualified Crm.Shared.Company        as C
import qualified Crm.Shared.Employee       as E
import qualified Crm.Shared.Task           as T
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
import qualified Crm.Shared.ServerRender   as SR
import           Crm.Shared.MyMaybe


instance (FromJSON a, Data a) => FromJSON (MyMaybe a) where
  parseJSON = fayInstance

instance (ToJSON a, Data a) => ToJSON (MyMaybe a) where
  toJSON = fromJust . showToFay

instance (JS.JSONSchema a) => JS.JSONSchema (MyMaybe a) where
  schema = gSchema

mkFayTransferables [''SR.Markup, ''M.ContactPersonForMachine, ''L.Login, ''MK.MachineKindSpecific, ''EF.ExtraFieldId,
  ''EF.ExtraFieldIdentification, ''C.Coordinates, ''U.UpkeepId, ''P.PhotoId, ''MT.MachineTypeId, ''M.MachineId, 
  ''E.EmployeeId, ''CP.ContactPersonId, ''C.CompanyId, ''MK.MachineKindEnum, ''US.UpkeepSequence, ''UM.UpkeepMachine, 
  ''U.Upkeep, ''MT.MyEither, ''MT.MachineType, ''D.Precision, ''D.YearMonthDay, ''M.Machine, ''C.Company, ''E.Employee,
  ''PM.PhotoMeta, ''CP.ContactPerson, ''T.Task, ''T.TaskId ]
