{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Data where

import "fay-base" Prelude

import qualified Crm.Shared.Machine as M
import qualified Crm.Shared.MachineType as MT
import qualified Crm.Shared.Company as C
import qualified Crm.Shared.Upkeep as U
import qualified Crm.Shared.UpkeepMachine as UM
import qualified Crm.Shared.YearMonthDay as YMD

data NavigationState =
  FrontPage {
    companiesNextService :: [(C.CompanyId, C.Company, Maybe YMD.YearMonthDay)] } | 
  CompanyDetail {
    companyId :: C.CompanyId , 
    company :: C.Company , 
    editing :: Bool , 
    companyMachines :: [(M.MachineId, M.Machine, C.CompanyId, MT.MachineTypeId, MT.MachineType)] } | 
  CompanyNew {
    company :: C.Company } | 
  NotFound | 
  MachineNew {
    machine :: M.Machine ,
    companyId :: C.CompanyId ,
    machineType :: MT.MachineType , 
    maybeMachineTypeId :: Maybe MT.MachineTypeId ,
    operationStartCalendarOpen :: Bool } | 
  MachineDetail {
    machine :: M.Machine ,
    machineType :: MT.MachineType , 
    machineTypeId :: MT.MachineTypeId ,
    operationStartCalendarOpen :: Bool , 
    formState :: Bool , 
    machineId :: M.MachineId , 
    machineNextService :: YMD.YearMonthDay } | 
  UpkeepNew {
    upkeep :: U.Upkeep , 
    upkeepMachines :: [(M.MachineId, M.Machine, C.CompanyId, MT.MachineTypeId, MT.MachineType)] , 
    notCheckedMachines :: [UM.UpkeepMachine] , 
    upkeepDatePickerOpen :: Bool , 
    companyId :: C.CompanyId } | 
  UpkeepClose {
    upkeep :: U.Upkeep , 
    machines :: [(M.MachineId, M.Machine, C.CompanyId, MT.MachineTypeId, MT.MachineType)] , 
    notCheckedMachines :: [UM.UpkeepMachine] , 
    upkeepDatePickerOpen :: Bool ,
    upkeepId :: U.UpkeepId ,
    companyId :: C.CompanyId } | 
  UpkeepHistory {
    companyUpkeeps :: [(U.UpkeepId,U.Upkeep)] } | 
  PlannedUpkeeps { 
    plannedUpkeeps :: [(U.UpkeepId,U.Upkeep,C.CompanyId,C.Company)] }

data AppState = AppState {
  navigation :: NavigationState }

defaultAppState :: AppState
defaultAppState = AppState {
  navigation = FrontPage [] }
