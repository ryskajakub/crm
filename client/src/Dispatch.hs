{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Dispatch where

import "fay-base" Prelude hiding (span, div, elem)
import "fay-base" Data.Var (Var, newVar, subscribeAndRead)

import Crm.Router (startRouter)
import qualified Crm.Component.Navigation as Navigation
import Crm.Page.Company (companiesList, companyDetail, companyNew)
import Crm.Page.ContactPerson (contactPersonForm, contactPersonsList)
import Crm.Page.Machine (machineNew, machineDetail)
import Crm.Page.Upkeep (upkeepNew, plannedUpkeeps, upkeepDetail)
import Crm.Page.UpkeepHistory (upkeepHistory)
import Crm.Page.MachineType (machineTypesList, machineTypeForm, machineTypePhase1Form)
import Crm.Page.Employee (employeePage, newEmployeeForm, employeeEdit)
import Crm.Page.NotFound (notFound)
import qualified Crm.Data.Data as D
import qualified Crm.Data.MachineData as MD
import qualified Crm.Data.UpkeepData as UD
import qualified Crm.Data.EmployeeData as ED

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
      D.MachineScreen (MD.MachineData machine machineTypeTuple operationStartCalendar 
          companyPersonId companyPersons machinePageMode) ->
        emptyCallback $ case machinePageMode of
          Left (MD.MachineDetail machineId nextService editing _ photos upkeeps companyId) ->
            machineDetail editing appVar' router companyId operationStartCalendar machine
              machineTypeTuple machineId nextService photos upkeeps companyPersonId companyPersons
          Right (MD.MachineNew companyId maybeMachineTypeId) ->
            machineNew router appVar' operationStartCalendar machine
              companyId machineTypeTuple maybeMachineTypeId companyPersonId companyPersons
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
      D.EmployeeManage (ED.EmployeeData employee (Just employeeId)) -> 
        emptyCallback $ employeeEdit employeeId router employee appVar'
      D.EmployeeManage (ED.EmployeeData employee _) -> emptyCallback $ newEmployeeForm router employee appVar'
      D.ContactPersonPage contactPerson identification -> 
        emptyCallback $ contactPersonForm contactPerson identification appVar'
      D.ContactPersonList contactPersons -> emptyCallback $ contactPersonsList contactPersons
    in Navigation.navigation' router newElementAndCallback )
  return ()

appVar :: Fay (Var D.AppState)
appVar = newVar D.defaultAppState
