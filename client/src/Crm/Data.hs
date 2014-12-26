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
    companyMachines :: [(Int, M.Machine, MT.MachineType)] } | 
  CompanyNew {
    company :: C.Company } | 
  NotFound | 
  MachineNew {
    machine :: (M.Machine, MT.MachineType) , 
    operationStartCalendarOpen :: Bool } | 
  MachineDetail {
    machine :: (M.Machine, MT.MachineType) , 
    operationStartCalendarOpen :: Bool , 
    formState :: Bool , 
    machineId :: Int , 
    machineNextService :: YMD.YearMonthDay } | 
  UpkeepNew {
    upkeep :: U.Upkeep , 
    upkeepMachines :: [(Int, M.Machine, MT.MachineType)] , 
    notCheckedMachines :: [UM.UpkeepMachine] , 
    upkeepDatePickerOpen :: Bool , 
    companyId :: Int } | 
  UpkeepHistory {
    companyUpkeeps :: [(Int,U.Upkeep)] } | 
  PlannedUpkeeps { 
    plennedUpkeeps :: [(Int, U.Upkeep, Int, C.Company)] }

data AppState = AppState {
  navigation :: NavigationState }

defaultAppState :: AppState
defaultAppState = AppState {
  navigation = FrontPage [] }
