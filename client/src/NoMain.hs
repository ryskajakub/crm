{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module NoMain where

import "fay-base" Prelude hiding (span, div, elem)
import Data.Var (Var, newVar, subscribeAndRead, modify, waitFor)

import Crm.Router (startRouter)
import Crm.Server (fetchCompanies, fetchMachines, fetchUpkeeps)
import qualified Crm.Component.Navigation as Navigation
import Crm.Component.Company (companiesList, companyDetail, companyNew)
import Crm.Component.Machine (machineNew, machineDetail)
import Crm.Component.Upkeep (upkeepNew, plannedUpkeeps)
import Crm.Component.UpkeepHistory (upkeepHistory)
import qualified Crm.Component.Data as D

main' :: Fay ()
main' = do
  appVar' <- appVar
  fetchCompanies (\companies' ->
    modify appVar' (\appState ->
      appState { D.companies = companies' }
    ))
  fetchMachines (\machines' ->
    modify appVar' (\appState ->
      appState { D.machines = machines' }
    ))
  fetchUpkeeps (\upkeeps' ->
    modify appVar' (\appState ->
      appState { D.upkeeps = upkeeps' }
    ))
  waitFor appVar' (\appState -> (not $ null $ D.machines appState) && (not $ null $ D.companies appState)) $
      \_ -> do
    router <- startRouter appVar'
    _ <- subscribeAndRead appVar' (\appState -> let
      frontPage data' = Navigation.navigation router (companiesList router data')
      in case D.navigation appState of
        D.FrontPage data' -> frontPage data'
        D.NotFound -> undefined
        D.CompanyDetail companyId' company' editing' machines' ->
          Navigation.navigation router
            (companyDetail editing' router appVar' (companyId', company') machines')
        D.CompanyNew company' -> Navigation.navigation router (companyNew router appVar' company')
        D.MachineNew machine' operationStartCalendarOpen' -> 
          Navigation.navigation router (machineNew router appVar' operationStartCalendarOpen' machine')
        D.MachineDetail machine' operationStartCalendarOpen' formState machineId' nextService ->
          Navigation.navigation router (machineDetail formState router
            appVar' operationStartCalendarOpen' machine' machineId' nextService)
        D.UpkeepNew upkeep' machines' notCheckedMachines' pickerOpen companyId' ->
          Navigation.navigation router 
            (upkeepNew router appVar' upkeep' pickerOpen notCheckedMachines' machines' companyId')
        D.UpkeepHistory upkeeps' -> Navigation.navigation router $ upkeepHistory upkeeps'
        D.PlannedUpkeeps plannedUpkeeps' -> Navigation.navigation router
          (plannedUpkeeps router plannedUpkeeps'))
    return ()
  return ()

appVar :: Fay (Var D.AppState)
appVar = newVar D.defaultAppState
