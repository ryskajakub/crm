{-# LANGUAGE PackageImports #-}

module Crm.Data.MachineData where

import "fay-base" Data.Text (Text)

import Crm.Shared.Company
import Crm.Shared.ContactPerson
import Crm.Shared.Machine
import Crm.Shared.MachineType
import Crm.Shared.MachineKind
import Crm.Shared.YearMonthDay
import Crm.Shared.UpkeepSequence
import Crm.Shared.Photo
import Crm.Shared.PhotoMeta
import Crm.Shared.Upkeep
import Crm.Shared.UpkeepMachine
import Crm.Shared.Employee

import Crm.Component.DatePicker
import qualified Crm.Validation as V

data MachineData = MachineData {
  machine :: (Machine, Text) ,
  machineKindSpecific :: MachineKindData ,
  machineTypeTuple :: (MachineType, [UpkeepSequence]) ,
  operationStartCalendar :: DatePicker ,
  contactPersonId :: Maybe ContactPersonId ,
  contactPersons :: [(ContactPersonId, ContactPerson)] ,
  validation :: V.Validation ,
  otherMachineId :: Maybe MachineId ,
  otherMachines :: [(MachineId, Machine)] ,
  machinePageMode :: Either MachineDetail MachineNew }

data MachineDetail = MachineDetail {
  machineId :: MachineId ,
  machineNextService :: YearMonthDay ,
  formState :: Bool ,
  machineTypeId :: MachineTypeId ,
  photos :: [(PhotoId, PhotoMeta)] ,
  upkeeps :: [(UpkeepId, Upkeep, UpkeepMachine, Maybe Employee)] ,
  companyId' :: CompanyId }

data MachineNew = MachineNew {
  companyId :: CompanyId ,
  maybeMachineTypeId :: Maybe MachineTypeId }
