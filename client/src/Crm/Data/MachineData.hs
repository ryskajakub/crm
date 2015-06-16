module Crm.Data.MachineData where

import           Data.Text (Text)

import           Crm.Shared.Company
import           Crm.Shared.ContactPerson
import           Crm.Shared.Machine        (Machine, MachineId)
import           Crm.Shared.MachineType
import           Crm.Shared.MachineKind
import           Crm.Shared.YearMonthDay
import           Crm.Shared.UpkeepSequence
import           Crm.Shared.Photo
import           Crm.Shared.PhotoMeta
import           Crm.Shared.Upkeep
import           Crm.Shared.UpkeepMachine
import           Crm.Shared.ExtraField

import           Crm.Component.DatePicker
import qualified Crm.Validation            as V

import           Crm.Component.Form        (InputState)

data ContactPersonInMachine = New | ById

data MachineData = MachineData {
  machine :: (Machine, Text) ,
  machineKindSpecific :: MachineKindEnum ,
  machineTypeTuple :: (MachineType, [UpkeepSequence]) ,
  operationStartCalendar :: DatePicker ,
  contactPersonId :: (ContactPerson, Maybe ContactPersonId, ContactPersonInMachine) ,
  contactPersons :: [(ContactPersonId, ContactPerson)] ,
  validation :: V.Validation ,
  otherMachineId :: Maybe MachineId ,
  otherMachines :: [(MachineId, Machine)] ,
  extraFields :: [(ExtraFieldId, MachineKindSpecific, Text)] ,
  machinePageMode :: Either MachineDetail MachineNew }

data MachineDetail = MachineDetail {
  machineId :: MachineId ,
  machineNextService :: YearMonthDay ,
  formState :: InputState ,
  machineTypeId :: MachineTypeId ,
  photos :: [(PhotoId, PhotoMeta)] ,
  upkeeps :: [(UpkeepId, Upkeep, UpkeepMachine)] ,
  companyId' :: CompanyId }

data MachineNew = MachineNew {
  companyId :: CompanyId ,
  maybeMachineTypeId :: Maybe MachineTypeId }
