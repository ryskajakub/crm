{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Dispatch where

import "fay-base" Prelude hiding (span, div, elem)
import "fay-base" Data.Var (Var, newVar, subscribeAndRead)
import "fay-base" Data.LocalStorage
import "fay-base" Data.Defined (fromDefined)
import "fay-base" Data.Text (unpack, fromString, (<>), showInt)
import "fay-base" Data.Maybe (onJust, joinMaybe)

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
import Crm.Helpers (parseSafely)

import qualified Crm.Shared.MachineType as MT
import qualified Crm.Shared.UpkeepSequence as US

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
      D.ContactPersonList contactPersons -> emptyCallback $ contactPersonsList router contactPersons
    in Navigation.navigation' router newElementAndCallback )
  return ()

loadFromLocalStorage :: Fay (Maybe (MT.MachineType, [US.UpkeepSequence], Maybe MT.MachineTypeId))
loadFromLocalStorage = do
  name <- getLocalStorage "mt.name"
  kind <- getLocalStorage "mt.kind"
  manufacturer <- getLocalStorage "mt.manufacturer"
  upkeepSequencesCount' <- getLocalStorage "us.length"
  case (fromDefined name, joinMaybe $ parseSafely `onJust` fromDefined kind, fromDefined manufacturer, 
      parseSafely `onJust` fromDefined upkeepSequencesCount') of
    (Just name', Just kind', Just manufacturer', Just upkeepSequencesCount) -> do
      mtId <- getLocalStorage "mt.id"
      let mtId' = MT.MachineTypeId `onJust` (joinMaybe $ parseSafely `onJust` fromDefined mtId)
      let machineType = MT.MachineType kind' (unpack name') (unpack manufacturer')
      seqs <- case upkeepSequencesCount of
        Just count -> do
          let 
            loadUpkeepSequence i = do
              let 
                index = showInt i
                parseBool text = case text of
                  _ | text == "True" -> Just $ True
                  _ | text == "False" -> Just $ True
                  _ -> Nothing
              displayOrdering <- getLocalStorage ("us." <> index <> ".displayOrdering") 
              label <- getLocalStorage ("us." <> index <> ".label") 
              repetition <- getLocalStorage ("us." <> index <> ".repetition") 
              oneTime <- getLocalStorage ("us." <> index <> ".oneTime") 
              return $ case (joinMaybe $ parseSafely `onJust` fromDefined displayOrdering, fromDefined label, joinMaybe $ 
                  parseSafely `onJust` fromDefined repetition, joinMaybe $ parseBool `onJust` fromDefined oneTime) of
                (Just d, Just l, Just r, Just o) -> Just $ US.UpkeepSequence d (unpack l) r o
                _ -> Nothing
          maybeBrokenUSs <- forM [0..(count - 1)] loadUpkeepSequence
          let 
            verifiedUSs = foldl (\acc elem -> case (acc, elem) of
              (Just acc', Just e) -> Just $ acc' ++ [e]
              _ -> Nothing) (Just []) maybeBrokenUSs
          return $ maybe [] id verifiedUSs
        Nothing -> return []

      return $ Just (machineType, seqs, mtId')
    _ -> return Nothing

appVar :: Fay (Var D.AppState)
appVar = do
  storedState <- loadFromLocalStorage
  let 
    appState = case storedState of
      Just (mt, seqs, mtId) -> D.defaultAppState {
        D.maybeMachineIdFromPhase1 = mtId ,
        D.machineTypeFromPhase1 = (mt, seqs) }
      _ -> D.defaultAppState
  newVar appState
