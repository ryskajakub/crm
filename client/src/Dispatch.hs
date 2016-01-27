{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Dispatch where

import           Prelude                   hiding (span, div, elem, id)
import qualified Prelude                   as P
import           Data.Var                  (Var, newVar, subscribeAndRead)
import           Data.LocalStorage
import           Data.Defined              (fromDefined)
import           Data.Text                 (fromString, (<>), showInt)
import           Data.Maybe                (onJust, joinMaybe)

import           HaskellReact              hiding (label)

import           Crm.RoutesHandler         (startRouter)
import qualified Crm.Component.Navigation  as Navigation
import           Crm.Page.Company          (companiesList, companyDetail, companyNew)
import           Crm.Page.ContactPerson    (contactPersonForm, contactPersonsList)
import           Crm.Page.Machine          (machineNew, machineDetail)
import           Crm.Page.MachineKind      (machineKindSettings)
import           Crm.Page.Upkeep           (upkeepNew, plannedUpkeeps, upkeepDetail)
import           Crm.Page.UpkeepHistory    (upkeepHistory)
import           Crm.Page.MachineType      (machineTypesList, machineTypeForm, machineTypePhase1Form)
import           Crm.Page.Employee         (employeePage, newEmployeeForm, employeeEdit, employeeTasks, employeeTask)
import           Crm.Page.UpkeepPhoto      (addPhotoToUpkeepList, upkeepPhotos)
import           Crm.Page.MachineSchema    (schema)
import           Crm.Page.NotFound         (notFound, serverDown)
import           Crm.Page.Dashboard        (dashboard)
import           Crm.Page.Login            (login)
import           Crm.Page.UpkeepPrint      (upkeepPrint)
import qualified Crm.Data.Data             as D
import qualified Crm.Data.MachineData      as MD
import qualified Crm.Data.UpkeepData       as UD
import qualified Crm.Data.EmployeeData     as ED
import           Crm.Helpers               (parseSafely)

import qualified Crm.Shared.MachineType    as MT
import qualified Crm.Shared.MachineKind    as MK
import qualified Crm.Shared.UpkeepSequence as US


emptyCallback :: a -> (a, Fay ())
emptyCallback element = (element, return ())

main' :: Fay ()
main' = do
  appVar' <- appVar
  router <- startRouter appVar'
  _ <- subscribeAndRead appVar' $ \appState -> let
    n = Navigation.navigation' router
    in case D.navigation appState of
      D.Dashboard companies -> n $ dashboard router companies
      D.FrontPage ordering data' -> n $ emptyCallback 
        (companiesList router (fst ordering) (snd ordering) data')
      D.NotFound -> n $ emptyCallback $ notFound
      D.ServerDown -> n $ emptyCallback $ serverDown
      D.CompanyDetail companyId' company' contactPersons editing' machines' lastUpkeep -> n $
        emptyCallback (companyDetail editing' router appVar' contactPersons (companyId', company') machines' lastUpkeep)
      D.CompanyNew company' -> n $ emptyCallback (companyNew router appVar' company')
      D.MachineScreen (MD.MachineData machine machineTypeTuple operationStartCalendar companyPersonId 
          companyPersons v otherMachineId otherMachines extraFields machineTypeId machinePageMode) ->
        n $ case machinePageMode of
          Left (MD.MachineDetail machineId nextService editing photos upkeeps companyId upkeepPhotoIds) ->
            machineDetail editing appVar' router companyId operationStartCalendar machine 
              machineTypeTuple machineId nextService photos upkeeps companyPersonId companyPersons v 
              otherMachineId otherMachines extraFields machineTypeId upkeepPhotoIds
          Right (MD.MachineNew companyId (contactPerson, contactPersonActiveRow)) -> 
            emptyCallback $ machineNew router appVar' operationStartCalendar machine
              companyId machineTypeTuple machineTypeId (contactPerson, companyPersonId, 
              contactPersonActiveRow) companyPersons v otherMachineId otherMachines extraFields
      D.UpkeepScreen (UD.UpkeepData (upkeep @ (u2,u3)) machines notCheckedMachines
        upkeepDatePicker employees selectedEmployees validation companyId upkeepPageMode) -> n $
          emptyCallback $ case upkeepPageMode of
            Left (UD.UpkeepClose upkeepId displayedNote) ->
              upkeepDetail router appVar' (upkeepId, u2, u3) upkeepDatePicker notCheckedMachines
                machines companyId employees selectedEmployees validation displayedNote
            Right (UD.UpkeepNew upkeepIdentification) ->
              upkeepNew router appVar' upkeep upkeepDatePicker notCheckedMachines machines
                upkeepIdentification companyId employees selectedEmployees validation
      D.UpkeepHistory upkeeps' machines'' companyId deletable photosInModal -> 
        n $ upkeepHistory upkeeps' machines'' companyId deletable photosInModal appVar' router
      D.PlannedUpkeeps plannedUpkeeps' -> n $ 
        (plannedUpkeeps router plannedUpkeeps')
      D.MachineTypeList machineTypes -> n $ emptyCallback (machineTypesList router machineTypes)
      D.MachineTypeEdit machineTypeId machinesCount machineType machines -> n $
        machineTypeForm router appVar' machineTypeId machineType machines machinesCount
      D.MachineNewPhase1 maybeMachineTypeId machineType companyId -> n $ machineTypePhase1Form 
        maybeMachineTypeId machineType appVar' router companyId
      D.EmployeeList employees -> n $ emptyCallback $ employeePage router employees
      D.EmployeeManage (ED.EmployeeData employee (Just employeeId) takenColours) -> n $
        emptyCallback $ employeeEdit employeeId router employee appVar' takenColours
      D.EmployeeManage (ED.EmployeeData employee _ takenColours) -> n $ 
        emptyCallback $ newEmployeeForm router employee appVar' takenColours
      D.ContactPersonPage contactPerson identification companyId -> n $
        emptyCallback $ contactPersonForm router contactPerson identification companyId appVar'
      D.ContactPersonList companyId contactPersons -> n $ emptyCallback $ 
        contactPersonsList companyId router contactPersons
      D.ExtraFields _ showSuccess editedKind allSettings -> n $ 
        emptyCallback $ machineKindSettings appVar' showSuccess editedKind allSettings router
      D.MachinesSchema machines -> n $ schema machines
      D.Login password wrongPassword -> let
        (body, callback) = emptyCallback $ login appVar' router password wrongPassword
        in simpleReactBody' body callback
      D.DailyPlan ymd employeeId dailyPlanData es ->
        n . emptyCallback $ upkeepPrint router appVar' ymd employeeId dailyPlanData es
      D.EmployeeTasksScreen (ED.EmployeeTasksData employeeId openTasks closedTasks) ->
        n . emptyCallback $ employeeTasks employeeId openTasks closedTasks router
      D.EmployeeTaskScreen employeeTaskData ->
        n . emptyCallback $ employeeTask appVar' router employeeTaskData
      D.AddPhotoToUpkeepList pu -> n . emptyCallback $ addPhotoToUpkeepList router pu
      D.AddPhotoToUpkeep upkeepId upkeep company -> n . emptyCallback $ 
        upkeepPhotos router upkeepId upkeep company
  return ()

loadFromLocalStorage :: Fay (Maybe (MT.MachineType, [US.UpkeepSequence], Maybe MT.MachineTypeId))
loadFromLocalStorage = do
  name <- getLocalStorage "mt.name"
  kind <- getLocalStorage "mt.kind"
  manufacturer <- getLocalStorage "mt.manufacturer"
  upkeepSequencesCount' <- getLocalStorage "us.length"
  case (fromDefined name, MK.dbReprToKind `onJust` (joinMaybe (parseSafely `onJust` fromDefined kind)), fromDefined manufacturer, 
      parseSafely `onJust` fromDefined upkeepSequencesCount') of
    (Just name', Just kind', Just manufacturer', Just upkeepSequencesCount) -> do
      mtId <- getLocalStorage "mt.id"
      let mtId' = MT.MachineTypeId `onJust` (joinMaybe $ parseSafely `onJust` fromDefined mtId)
      let machineType = MT.MachineType kind' name' manufacturer'
      seqs <- case upkeepSequencesCount of
        Just count -> do
          let 
            loadUpkeepSequence i = do
              let 
                index = showInt i
                parseBool text = case text of
                  _ | text == "True"  -> Just $ True
                  _ | text == "False" -> Just $ False
                  _ -> Nothing
              displayOrdering <- getLocalStorage ("us." <> index <> ".displayOrdering") 
              label <- getLocalStorage ("us." <> index <> ".label") 
              repetition <- getLocalStorage ("us." <> index <> ".repetition") 
              oneTime <- getLocalStorage ("us." <> index <> ".oneTime") 
              return $ case (joinMaybe $ parseSafely `onJust` fromDefined displayOrdering, fromDefined label, joinMaybe $ 
                  parseSafely `onJust` fromDefined repetition, joinMaybe $ parseBool `onJust` fromDefined oneTime) of
                (Just d, Just l, Just r, Just o) -> Just $ US.UpkeepSequence d l r o
                _ -> Nothing
          maybeBrokenUSs <- forM [0..(count - 1)] loadUpkeepSequence
          let 
            verifiedUSs = foldl (\acc elem -> case (acc, elem) of
              (Just acc', Just e) -> Just $ acc' ++ [e]
              _ -> Nothing) (Just []) maybeBrokenUSs
          return $ maybe [] P.id verifiedUSs
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
