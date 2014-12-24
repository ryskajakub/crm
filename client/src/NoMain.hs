{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module NoMain where

import "fay-base" Prelude hiding (span, div, elem)
import Data.Var (Var, newVar, subscribeAndRead, get, modify, waitFor)
import FFI (ffi, Nullable)
import "fay-base" Data.Text (Text, pack)
import "fay-base" Data.Maybe (isJust)

import Moment (now, requireMoment, day)

import Crm.Router (startRouter)
import Crm.Helpers (parseSafely)
import Crm.Server (fetchCompanies, fetchMachines, fetchUpkeeps, fetchMachine, fetchPlannedUpkeeps)
import qualified Crm.Component.Navigation as Navigation
import Crm.Component.Company (companiesList, companyDetail, companyNew)
import Crm.Component.Machine (machineNew, machineDetail)
import Crm.Component.Upkeep (upkeepNew, plannedUpkeeps)
import Crm.Component.UpkeepHistory (upkeepHistory)
import Crm.Component.Data as D
import qualified Crm.Shared.Machine as M
import qualified Crm.Shared.Company as C
import qualified Crm.Shared.Upkeep as U
import qualified Crm.Shared.YearMonthDay as YMD
import qualified Crm.Shared.UpkeepMachine as UM
import qualified Crm.Shared.MachineType as MT

main' :: Fay ()
main' = do
  appVar' <- appVar
  fetchCompanies (\companies' ->
    modify appVar' (\appState ->
      appState { companies = companies' }
    ))
  fetchMachines (\machines' ->
    modify appVar' (\appState ->
      appState { machines = machines' }
    ))
  fetchUpkeeps (\upkeeps' ->
    modify appVar' (\appState ->
      appState { upkeeps = upkeeps' }
    ))
  waitFor appVar' (\appState -> (not $ null $ machines appState) && (not $ null $ companies appState)) $
      \_ -> do
    router <- startRouter appVar'
    _ <- subscribeAndRead appVar' (\appState -> let
      frontPage data' = Navigation.navigation router (companiesList router data')
      in case navigation appState of
        FrontPage data' -> frontPage data'
        NotFound -> undefined
        CompanyDetail companyId' company' editing' machines' ->
          Navigation.navigation router
            (companyDetail editing' router appVar' (companyId', company') machines')
        CompanyNew company' -> Navigation.navigation router (companyNew router appVar' company')
        MachineNew machine' operationStartCalendarOpen' -> 
          Navigation.navigation router (machineNew router appVar' operationStartCalendarOpen' machine')
        MachineDetail machine' operationStartCalendarOpen' formState machineId' nextService ->
          Navigation.navigation router (machineDetail formState router
            appVar' operationStartCalendarOpen' machine' machineId' nextService)
        UpkeepNew upkeep' machines' notCheckedMachines' pickerOpen companyId' ->
          Navigation.navigation router 
            (upkeepNew router appVar' upkeep' pickerOpen notCheckedMachines' machines' companyId')
        UpkeepHistory upkeeps' -> Navigation.navigation router $ upkeepHistory upkeeps'
        PlannedUpkeeps plannedUpkeeps' -> Navigation.navigation router
          (plannedUpkeeps router plannedUpkeeps'))
    return ()
  return ()

appVar :: Fay (Var AppState)
appVar = newVar defaultAppState
