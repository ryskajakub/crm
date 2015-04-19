{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Dispatch where

import "fay-base" Prelude hiding (span, div, elem)
import "fay-base" Data.Var (Var, newVar, subscribeAndRead)

import Crm.Router (startRouter)
import qualified Crm.Component.Navigation as Navigation
import Crm.Page.Company (companiesList, companyDetail, companyNew)
import Crm.Page.Machine (machineNew, machineDetail)
import Crm.Page.Upkeep (upkeepNew, plannedUpkeeps, upkeepDetail)
import Crm.Page.UpkeepHistory (upkeepHistory)
import Crm.Page.MachineType (machineTypesList, machineTypeForm, machineTypePhase1Form)
import Crm.Page.Employee (employeePage, employeeForm)
import Crm.Page.NotFound (notFound)
import qualified Crm.Data.Data as D
import qualified Crm.Data.MachineData as MD
import qualified Crm.Data.UpkeepData as UD

emptyCallback :: a -> (a, Fay ())
emptyCallback element = (element, return ())

main' :: Fay ()
main' = do
  appVar' <- appVar
  router <- startRouter appVar'
  _ <- subscribeAndRead appVar' (\appState -> let
    newElementAndCallback = case D.navigation appState of
      D.FrontPage ordering data' -> emptyCallback 
        (companiesList router (fst ordering) (snd ordering) data')
      D.NotFound -> emptyCallback $ notFound
      D.CompanyDetail companyId' company' editing' machines' ->
        emptyCallback (companyDetail editing' router appVar' (companyId', company') machines')
      D.CompanyNew company' -> emptyCallback (companyNew router appVar' company')
      D.MachineScreen (MD.MachineData machine machineTypeTuple operationStartCalendar machinePageMode) ->
        case machinePageMode of
          Left (MD.MachineDetail machineId nextService editing machineTypeId photos upkeeps) ->
            machineDetail editing appVar' operationStartCalendar machine machineTypeId
              machineTypeTuple machineId nextService photos upkeeps
          Right (MD.MachineNew companyId maybeMachineTypeId) ->
            machineNew router appVar' operationStartCalendar machine 
              companyId machineTypeTuple maybeMachineTypeId
      D.UpkeepScreen (UD.UpkeepData (upkeep @ (u2,u3)) machines notCheckedMachines
        upkeepDatePicker employees selectedEmployee upkeepPageMode) ->
          emptyCallback $ case upkeepPageMode of
            Left (UD.UpkeepClose upkeepId companyId) ->
              upkeepDetail router appVar' (upkeepId, u2, u3) upkeepDatePicker notCheckedMachines
                machines companyId employees selectedEmployee
            Right (UD.UpkeepNew upkeepIdentification) ->
              upkeepNew router appVar' upkeep upkeepDatePicker notCheckedMachines machines
                upkeepIdentification employees selectedEmployee
      D.UpkeepHistory upkeeps' companyId -> emptyCallback $ upkeepHistory upkeeps' companyId router
      D.PlannedUpkeeps plannedUpkeeps' -> emptyCallback
        (plannedUpkeeps router plannedUpkeeps')
      D.MachineTypeList machineTypes -> emptyCallback (machineTypesList router machineTypes)
      D.MachineTypeEdit machineTypeId machineType -> 
        machineTypeForm appVar' machineTypeId machineType 
      D.MachineNewPhase1 maybeMachineTypeId machineType companyId -> machineTypePhase1Form 
        maybeMachineTypeId machineType appVar' router companyId
      D.EmployeeList employees -> emptyCallback $ employeePage router employees
      D.EmployeeNew employee -> emptyCallback $ employeeForm router employee appVar'
    in Navigation.navigation' router newElementAndCallback )
  return ()

appVar :: Fay (Var D.AppState)
appVar = newVar D.defaultAppState
