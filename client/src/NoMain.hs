{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module NoMain where

import "fay-base" Prelude hiding (span, div, elem)
import Data.Var (Var, newVar, subscribeAndRead)

import Crm.Router (startRouter)
import qualified Crm.Component.Navigation as Navigation
import Crm.Page.Company (companiesList, companyDetail, companyNew)
import Crm.Page.Machine (machineNew, machineDetail)
import Crm.Page.Upkeep (upkeepNew, plannedUpkeeps, upkeepDetail)
import Crm.Page.UpkeepHistory (upkeepHistory)
import qualified Crm.Data as D

main' :: Fay ()
main' = do
  appVar' <- appVar
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
      D.MachineNew machine' machineType maybeMachineTypeId operationStartCalendarOpen' -> 
        Navigation.navigation' router (machineNew router appVar' operationStartCalendarOpen' machine' machineType)
      D.MachineDetail machine' machineType machineTypeId operationStartCalendarOpen' editing machineId' nextService ->
        Navigation.navigation' router (machineDetail editing router
          appVar' operationStartCalendarOpen' machine' machineTypeId machineType machineId' nextService)
      D.UpkeepNew upkeep' machines' notCheckedMachines' pickerOpen companyId' ->
        Navigation.navigation router 
          (upkeepNew router appVar' upkeep' pickerOpen notCheckedMachines' machines' companyId')
      D.UpkeepHistory upkeeps' -> Navigation.navigation router $ upkeepHistory upkeeps'
      D.PlannedUpkeeps plannedUpkeeps' -> Navigation.navigation router
        (plannedUpkeeps router plannedUpkeeps')
      D.UpkeepClose upkeep machines notCheckedMachines upkeepDatePickerOpen upkeepId companyId
        -> Navigation.navigation router 
          (upkeepDetail router appVar' upkeep upkeepDatePickerOpen notCheckedMachines
            machines upkeepId companyId))
  return ()

appVar :: Fay (Var D.AppState)
appVar = newVar D.defaultAppState
