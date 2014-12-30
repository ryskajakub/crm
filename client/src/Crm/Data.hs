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
    companiesNextService :: [(Int, C.Company, Maybe YMD.YearMonthDay)] } | 
  CompanyDetail {
    companyId :: Int , 
    company :: C.Company , 
    editing :: Bool , 
    companyMachines :: [(Int, M.Machine, Int, MT.MachineType)] } | 
  CompanyNew {
    company :: C.Company } | 
  NotFound | 
  MachineNew {
    machine :: M.Machine ,
    machineType :: MT.MachineType , 
    maybeMachineTypeId :: Maybe Int ,
    operationStartCalendarOpen :: Bool } | 
  MachineDetail {
    machine :: M.Machine ,
    machineType :: MT.MachineType , 
    machineTypeId :: Int ,
    operationStartCalendarOpen :: Bool , 
    formState :: Bool , 
    machineId :: Int , 
    machineNextService :: YMD.YearMonthDay } | 
  UpkeepNew {
    upkeep :: U.Upkeep , 
    upkeepMachines :: [(Int, M.Machine, Int, MT.MachineType)] , 
    notCheckedMachines :: [UM.UpkeepMachine] , 
    upkeepDatePickerOpen :: Bool , 
    companyId :: Int } | 
  UpkeepClose {
    upkeep :: U.Upkeep } |
  UpkeepHistory {
    companyUpkeeps :: [(Int,U.Upkeep)] } | 
  PlannedUpkeeps { 
    plennedUpkeeps :: [(Int, U.Upkeep, Int, C.Company)] }

data AppState = AppState {
  navigation :: NavigationState }

defaultAppState :: AppState
defaultAppState = AppState {
  navigation = FrontPage [] }
