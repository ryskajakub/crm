module Crm.Data.UpkeepData where

import           Crm.Shared.Company
import           Crm.Shared.Machine
import           Crm.Shared.MachineType
import           Crm.Shared.UpkeepMachine
import           Crm.Shared.Upkeep
import           Crm.Shared.UpkeepSequence
import           Crm.Shared.Employee

import           Crm.Component.DatePicker

import qualified Crm.Validation            as V
import           Crm.Types                 (DisplayedNote)

data UpkeepData = UpkeepData {
  upkeep :: (Upkeep, [UpkeepMachine']) ,
  machines :: [(MachineId, Machine, MachineType, UpkeepSequence)] ,
  notCheckedMachines :: [UpkeepMachine'] ,
  upkeepDatePicker :: DatePickerData ,
  employees :: [Employee'] ,
  selectedEmployees :: [Maybe EmployeeId] ,
  validation :: V.Validation ,
  companyId :: CompanyId ,
  upkeepPageMode :: Either UpkeepClose UpkeepNew }

data UpkeepNew = UpkeepNew {
  upkeepIdentification :: Maybe UpkeepId }

data UpkeepClose = UpkeepClose {
  upkeepId :: UpkeepId ,
  displayedNote :: DisplayedNote }
