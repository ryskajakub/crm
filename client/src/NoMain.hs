{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module NoMain where

import "fay-base" Prelude hiding (span, div, elem)
import "fay-base" Data.Var (Var, newVar, subscribeAndRead)

import Crm.Router (startRouter)
import qualified Crm.Component.Navigation as Navigation
import Crm.Page.Company (companiesList, companyDetail, companyNew)
import Crm.Page.Machine (machineNew, machineDetail, machineTypePhase1Form)
import Crm.Page.Upkeep (upkeepNew, plannedUpkeeps, upkeepDetail)
import Crm.Page.UpkeepHistory (upkeepHistory)
import Crm.Page.Other.MachineType (machineTypesList, machineTypeForm)
import qualified Crm.Data as D

emptyCallback :: a -> (a, Fay ())
emptyCallback element = (element, return ())

main' :: Fay ()
main' = do
  appVar' <- appVar
  router <- startRouter appVar'
  _ <- subscribeAndRead appVar' (\appState -> let
    newElementAndCallback = case D.navigation appState of
      D.FrontPage data' -> emptyCallback (companiesList router data')
      D.NotFound -> undefined
      D.CompanyDetail companyId' company' editing' machines' ->
        emptyCallback (companyDetail editing' router appVar' (companyId', company') machines')
      D.CompanyNew company' -> emptyCallback (companyNew router appVar' company')
      D.MachineNew machine' companyId machineType maybeMachineTypeId operationStartCalendarOpen' -> 
        (machineNew router appVar' operationStartCalendarOpen' 
          machine' companyId machineType maybeMachineTypeId)
      D.MachineDetail machine' machineType machineTypeId 
        operationStartCalendarOpen' editing machineId' nextService ->
          (machineDetail editing appVar' operationStartCalendarOpen' machine' 
            machineTypeId machineType machineId' nextService)
      D.UpkeepNew upkeep' machines' notCheckedMachines' pickerOpen companyId' es selectedE ->
        emptyCallback (upkeepNew router appVar' upkeep' 
          pickerOpen notCheckedMachines' machines' companyId' es selectedE)
      D.UpkeepHistory upkeeps' -> emptyCallback $ upkeepHistory upkeeps'
      D.PlannedUpkeeps plannedUpkeeps' -> emptyCallback
        (plannedUpkeeps router plannedUpkeeps')
      D.UpkeepClose upkeep machines notCheckedMachines upkeepDatePickerOpen 
          upkeepId companyId employees selectedEmployee -> let
        (u2, u3) = upkeep
        upkeep3 = (upkeepId, u2, u3)
        in emptyCallback
          (upkeepDetail router appVar' upkeep3 upkeepDatePickerOpen 
            notCheckedMachines machines companyId employees selectedEmployee)
      D.MachineTypeList machineTypes -> emptyCallback (machineTypesList router machineTypes)
      D.MachineTypeEdit machineType -> machineTypeForm appVar' machineType 
      D.MachineNewPhase1 maybeMachineTypeId machineType companyId -> machineTypePhase1Form 
        maybeMachineTypeId machineType appVar' router companyId
    in Navigation.navigation' router newElementAndCallback )
  return ()

appVar :: Fay (Var D.AppState)
appVar = newVar D.defaultAppState
