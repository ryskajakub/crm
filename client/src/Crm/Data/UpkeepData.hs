module Crm.Data.UpkeepData where

import Crm.Shared.Company
import Crm.Shared.Machine
import Crm.Shared.MachineType
import Crm.Shared.UpkeepMachine
import Crm.Shared.Upkeep
import Crm.Shared.Employee

import Crm.Component.DatePicker

data UpkeepData = UpkeepData {
  upkeep :: (Upkeep, [UpkeepMachine']) ,
  machines :: [(MachineId, Machine, CompanyId, MachineTypeId, MachineType)] ,
  notCheckedMachines :: [UpkeepMachine'] ,
  upkeepDatePicker :: DatePicker ,
  employees :: [Employee'] ,
  selectedEmployee :: Maybe EmployeeId ,
  upkeepPageMode :: Either UpkeepClose UpkeepNew }

data UpkeepNew = UpkeepNew {
  upkeepIdentification :: Either CompanyId UpkeepId }

data UpkeepClose = UpkeepClose {
  upkeepId :: UpkeepId ,
  companyId :: CompanyId }
