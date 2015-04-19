module Crm.Data.MachineData where

import Crm.Shared.Company
import Crm.Shared.Machine
import Crm.Shared.MachineType
import Crm.Shared.YearMonthDay
import Crm.Shared.UpkeepSequence
import Crm.Shared.Photo
import Crm.Shared.PhotoMeta
import Crm.Shared.Upkeep
import Crm.Shared.UpkeepMachine
import Crm.Shared.Employee

import Crm.Component.DatePicker

data MachineData = MachineData {
  machine :: Machine ,
  machineTypeTuple :: (MachineType, [UpkeepSequence]) ,
  operationStartCalendar :: DatePicker ,
  machinePageMode :: Either MachineDetail MachineNew }

data MachineDetail = MachineDetail {
  machineId :: MachineId ,
  machineNextService :: YearMonthDay ,
  formState :: Bool ,
  machineTypeId :: MachineTypeId ,
  photos :: [(PhotoId, PhotoMeta)] ,
  upkeeps :: [(Upkeep, UpkeepMachine, Maybe Employee)] }

data MachineNew = MachineNew {
  companyId :: CompanyId ,
  maybeMachineTypeId :: Maybe MachineTypeId }
