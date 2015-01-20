{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Router (
  startRouter ,
  navigate ,
  link ,
  CrmRouter ,
  CrmRoute ,
  frontPage ,
  newCompany ,
  companyDetail ,
  newMachinePhase2 ,
  newMachinePhase1 ,
  newMaintenance ,
  closeUpkeep ,
  replanUpkeep ,
  maintenances ,
  plannedUpkeeps ,
  machineTypesList ,
  machineTypeEdit ,
  machineDetail ) where

import "fay-base" Data.Text (fromString, showInt, Text, (<>))
import "fay-base" Prelude hiding (div, span, id)
import "fay-base" FFI (Automatic)
import "fay-base" Data.Var (Var, modify, get)
import "fay-base" Data.Function (fmap)
import "fay-base" Data.Maybe (fromJust)

import qualified HaskellReact.BackboneRouter as BR
import HaskellReact
import Moment (now, requireMoment, day)

import Crm.Server (fetchMachine, fetchPlannedUpkeeps, fetchFrontPageData, fetchEmployees,
  fetchCompany, fetchUpkeeps, fetchUpkeep, fetchMachineTypes, fetchMachineTypeById)
import Crm.Helpers (parseSafely, showCompanyId)
import qualified Crm.Shared.Machine as M
import qualified Crm.Shared.MachineType as MT
import qualified Crm.Shared.UpkeepMachine as UM
import qualified Crm.Shared.Upkeep as U
import qualified Crm.Shared.Company as C
import qualified Crm.Shared.YearMonthDay as YMD
import qualified Crm.Data.Data as D
import Crm.Data.MachineData (MachineData(MachineData), MachineNew(MachineNew)
  , MachineDetail(MachineDetail))
import Crm.Data.UpkeepData

newtype CrmRouter = CrmRouter BR.BackboneRouter
newtype CrmRoute = CrmRoute Text

frontPage :: CrmRoute
frontPage = CrmRoute ""

newCompany :: CrmRoute
newCompany = CrmRoute "companies/new"

companyDetail :: C.CompanyId -> CrmRoute
companyDetail companyId = CrmRoute $ "companies/" <> showCompanyId companyId

newMachinePhase1 :: C.CompanyId -> CrmRoute
newMachinePhase1 companyId = CrmRoute $ "companies/" <> showCompanyId companyId <> "/new-machine-phase1"

newMachinePhase2 :: C.CompanyId -> CrmRoute
newMachinePhase2 companyId = CrmRoute $ "companies/" <> showCompanyId companyId <> "/new-machine-phase2"

newMaintenance :: C.CompanyId -> CrmRoute
newMaintenance companyId = CrmRoute $ "companies/" <> showCompanyId companyId <> "/new-maintenance"

maintenances :: C.CompanyId -> CrmRoute
maintenances companyId = CrmRoute $ "companies/" <> showCompanyId companyId <> "/maintenances"

machineDetail :: M.MachineId -> CrmRoute
machineDetail machineId = CrmRoute $ "machines/" <> (showInt $ M.getMachineId machineId)

plannedUpkeeps :: CrmRoute
plannedUpkeeps = CrmRoute "planned"

closeUpkeep :: U.UpkeepId -> CrmRoute
closeUpkeep upkeepId = CrmRoute $ "upkeeps/" <> (showInt $ U.getUpkeepId upkeepId)

replanUpkeep :: U.UpkeepId -> CrmRoute
replanUpkeep upkeepId = CrmRoute $ "upkeeps/" <> (showInt $ U.getUpkeepId upkeepId) <> "/replan"

machineTypesList :: CrmRoute
machineTypesList = CrmRoute "other/machine-types-list" 

machineTypeEdit :: MT.MachineTypeId -> CrmRoute
machineTypeEdit machineTypeId = CrmRoute $ "machine-types/"  <> (showInt $ MT.getMachineTypeId machineTypeId)

startRouter :: Var D.AppState -> Fay CrmRouter
startRouter appVar = let
  modify' newState = modify appVar (\appState -> appState { D.navigation = newState })
  withCompany :: [Text]
              -> (C.CompanyId -> (C.Company, [(M.MachineId, M.Machine,
                   C.CompanyId, MT.MachineTypeId, MT.MachineType)]) 
                 -> D.NavigationState)
              -> Fay ()
  withCompany params newStateFun = case parseSafely $ head params of
    Just(companyId') -> let
      companyId = C.CompanyId companyId'
      in fetchCompany companyId (\data' -> let
        newState = newStateFun companyId data'
        in modify' newState )
    _ -> modify' D.NotFound 
  (nowYear, nowMonth, nowDay) = day $ now requireMoment
  nowYMD = YMD.YearMonthDay nowYear nowMonth nowDay YMD.DayPrecision
  in fmap CrmRouter $ BR.startRouter [(
    "", const $ fetchFrontPageData (\data' ->
      modify appVar (\appState -> appState { D.navigation = D.FrontPage data' }))
  ),(
    "companies/:id", \params -> let
      cId = head params
      in case (parseSafely cId, cId) of
        (Just(cId''), _) -> let
          companyId = C.CompanyId cId''
          in fetchCompany companyId (\(company,machines) -> 
            modify appVar (\appState -> appState {
              D.navigation = D.CompanyDetail companyId company False machines }))
        (_, new) | new == "new" -> modify appVar (\appState ->
          appState {
            D.navigation = D.CompanyNew C.newCompany }
          )
        _ -> return ()
  ),(
    "companies/:id/new-machine-phase1", \params ->
    withCompany
      params
      (\companyId (_,_) ->
        D.MachineNewPhase1 Nothing (MT.newMachineType,[]) companyId)
  ),(
    "companies/:id/new-machine-phase2", \params -> let
      cId = head params
      in case (parseSafely cId) of
        Just companyIdInt -> do
          appState <- get appVar
          let
            machineTypeTuple = D.machineTypeFromPhase1 appState
            maybeMachineTypeId = D.maybeMachineIdFromPhase1 appState
            companyId = C.CompanyId companyIdInt
          modify' $ 
            D.MachineScreen $ MachineData (M.newMachine nowYMD) machineTypeTuple (nowYMD, False)
              (Right $ MachineNew companyId maybeMachineTypeId)
        _ -> modify' D.NotFound
  ),(
    "companies/:id/new-maintenance", \params ->
      fetchEmployees (\employees -> 
        withCompany
          params
          (\companyId (_, machines) -> let
            notCheckedUpkeepMachines = map (\(machineId,_,_,_,_) -> 
              (UM.newUpkeepMachine, machineId)) machines
            in D.UpkeepScreen $ UpkeepData (U.newUpkeep nowYMD, []) machines notCheckedUpkeepMachines
              (nowYMD, False) employees Nothing (Right $ UpkeepNew $ Left companyId)))
  ),(
    "companies/:id/maintenances", \params ->
      case (parseSafely $ head params) of
        Just(companyId) -> 
          fetchUpkeeps (C.CompanyId companyId) (\result -> let
            ns = D.UpkeepHistory result
            in modify' ns)
        _ -> modify' D.NotFound
  ),(
    "machines/:id", \params -> let
      maybeId = parseSafely $ head params
      in case maybeId of
        Just(machineId') -> let
          machineId = M.MachineId machineId'
          in fetchMachine machineId
            (\(machine, machineTypeId, _, machineTypeTuple, machineNextService) ->
              modify' $ D.MachineScreen $ MachineData machine machineTypeTuple (nowYMD,False) $
                (Left $ MachineDetail machineId machineNextService False machineTypeId))
        _ -> modify' D.NotFound
  ),(
    "planned", const $
      fetchPlannedUpkeeps (\plannedUpkeeps' -> let
        newNavigation = D.PlannedUpkeeps plannedUpkeeps'
        in modify appVar (\appState -> 
          appState { D.navigation = newNavigation })) 
  ),(
    "upkeeps/:id", \params -> let
      maybeId = parseSafely $ head params
      in case maybeId of
        Just(upkeepId') -> let 
          upkeepId = U.UpkeepId upkeepId'
          in fetchUpkeep upkeepId (\(companyId,(upkeep,selectedEmployee,upkeepMachines),machines) -> 
            fetchEmployees (\employees -> let
              upkeep' = upkeep { U.upkeepClosed = True }
              upkeepDate = U.upkeepDate upkeep
              in modify' $ D.UpkeepScreen $ UpkeepData (upkeep', upkeepMachines) machines
                (notCheckedMachines' machines upkeepMachines) (upkeepDate, False) employees 
                selectedEmployee (Left $ UpkeepClose upkeepId companyId)))
        _ -> modify' D.NotFound 
  ),(
    "other/machine-types-list", const $
      fetchMachineTypes (\result -> modify' $ D.MachineTypeList result )
  ),(
    "machine-types/:id", \params -> let
      maybeId = parseSafely $ head params
      in case maybeId of
        Just (machineTypeIdInt) -> let
          machineTypeId = (MT.MachineTypeId machineTypeIdInt)
          in fetchMachineTypeById machineTypeId ((\(_,machineType, upkeepSequences) ->
            modify' $ D.MachineTypeEdit machineTypeId (machineType, upkeepSequences) ) . fromJust) 
        _ -> modify' D.NotFound 
  ),(
    "upkeeps/:id/replan", \params -> let
      upkeepId'' = parseSafely $ head params
      in case upkeepId'' of
        Just (upkeepId') -> let
          upkeepId = U.UpkeepId upkeepId'
          in fetchUpkeep upkeepId (\(_, (upkeep, employeeId, upkeepMachines), machines) ->
            fetchEmployees (\employees ->
              modify' $ D.UpkeepScreen $ UpkeepData (upkeep, upkeepMachines) machines
                (notCheckedMachines' machines upkeepMachines) (U.upkeepDate upkeep, False)
                employees employeeId (Right $ UpkeepNew $ Right upkeepId)))
        _ -> modify' D.NotFound)]

notCheckedMachines' :: [(M.MachineId,t1,t2,t3,t4)] -> [(t5,M.MachineId)] -> [(UM.UpkeepMachine, M.MachineId)]
notCheckedMachines' machines upkeepMachines = let 
  addNotCheckedMachine acc element = let 
    (machineId,_,_,_,_) = element
    machineChecked = find (\(_,machineId') -> 
      machineId == machineId') upkeepMachines
    in case machineChecked of
      Nothing -> (UM.newUpkeepMachine,machineId) : acc
      _ -> acc
  in foldl addNotCheckedMachine [] machines

navigate :: CrmRoute
         -> CrmRouter
         -> Fay ()
navigate (CrmRoute route) (CrmRouter router) = BR.navigate route router

link :: Renderable a
     => Automatic a
     -> CrmRoute
     -> CrmRouter
     -> DOMElement
link children (CrmRoute route) (CrmRouter router) = 
  BR.link children route router
