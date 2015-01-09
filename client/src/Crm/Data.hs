{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Data where

import "fay-base" Prelude
import "fay-base" Data.Var (Var, modify)

import qualified Crm.Shared.Machine as M
import qualified Crm.Shared.MachineType as MT
import qualified Crm.Shared.Company as C
import qualified Crm.Shared.Upkeep as U
import qualified Crm.Shared.Employee as E
import qualified Crm.Shared.UpkeepMachine as UM
import qualified Crm.Shared.UpkeepSequence as US
import qualified Crm.Shared.YearMonthDay as YMD

import qualified Crm.Component.DatePicker as DP

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
    machineTypeTuple :: (MT.MachineType,[US.UpkeepSequence]) , 
    maybeMachineTypeId :: Maybe MT.MachineTypeId ,
    operationStartCalendar :: DP.DatePicker } | 
  MachineDetail {
    machine :: M.Machine ,
    machineType :: MT.MachineType , 
    machineTypeId :: MT.MachineTypeId ,
    operationStartCalendar :: DP.DatePicker , 
    formState :: Bool , 
    machineId :: M.MachineId , 
    machineNextService :: YMD.YearMonthDay } | 
  UpkeepNew {
    upkeep :: (U.Upkeep, [UM.UpkeepMachine']) , 
    upkeepMachines :: [(M.MachineId, M.Machine, C.CompanyId, MT.MachineTypeId, MT.MachineType)] , 
    notCheckedMachines :: [UM.UpkeepMachine'] , 
    upkeepDatePicker :: DP.DatePicker , 
    companyId :: C.CompanyId ,
    employees :: [E.Employee'] ,
    selectedEmployee :: Maybe E.EmployeeId } | 
  UpkeepClose {
    upkeep :: (U.Upkeep, [UM.UpkeepMachine']) , 
    machines :: [(M.MachineId, M.Machine, C.CompanyId, MT.MachineTypeId, MT.MachineType)] , 
    notCheckedMachines :: [UM.UpkeepMachine'] , 
    upkeepDatePicker :: DP.DatePicker ,
    upkeepId :: U.UpkeepId ,
    companyId :: C.CompanyId ,
    employees :: [E.Employee'] ,
    selectedEmployee :: Maybe E.EmployeeId } | 
  UpkeepHistory {
    companyUpkeeps :: [U.Upkeep''] } | 
  PlannedUpkeeps { 
    plannedUpkeeps :: [(U.UpkeepId,U.Upkeep,C.CompanyId,C.Company)] } |
  MachineTypeList {
    machineTypes :: [(MT.MachineType',Int)] } |
  MachineTypeEdit {
    machineTypeId :: MT.MachineTypeId ,
    machineTypeTuple :: (MT.MachineType, [US.UpkeepSequence]) } |
  MachineNewPhase1 {
    maybeMachineTypeId :: Maybe MT.MachineTypeId ,
    machineTypeTuple :: (MT.MachineType, [US.UpkeepSequence]) ,
    companyId :: C.CompanyId }

data AppState = AppState {
  navigation :: NavigationState ,
  machineTypeFromPhase1 :: (MT.MachineType, [US.UpkeepSequence]) ,
  maybeMachineIdFromPhase1 :: Maybe MT.MachineTypeId }

modifyState :: Var AppState -> (NavigationState -> NavigationState) -> Fay ()
modifyState var fun = modify var (\appState' -> appState' { navigation = fun $ navigation appState' } )

defaultAppState :: AppState
defaultAppState = AppState {
  navigation = FrontPage [] ,
  machineTypeFromPhase1 = (MT.newMachineType,[]) ,
  maybeMachineIdFromPhase1 = Nothing }
