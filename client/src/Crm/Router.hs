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
  dashboard ,
  frontPage ,
  defaultFrontPage ,
  newCompany ,
  companyDetail ,
  newMachinePhase2 ,
  newMachinePhase1 ,
  newMaintenance ,
  closeUpkeep ,
  replanUpkeep ,
  maintenances ,
  newContactPerson ,
  plannedUpkeeps ,
  machineTypesList ,
  machineTypeEdit ,
  machineDetail ,
  employeePage ,
  newEmployee ,
  contactPersonList ,
  contactPersonEdit ,
  extraFields ,
  editEmployee ) where

import "fay-base" Data.Text (fromString, showInt, Text, (<>), unpack)
import "fay-base" Prelude hiding (div, span) 
import "fay-base" Data.Var (Var, modify, get)
import "fay-base" Data.Function (fmap)
import "fay-base" Data.Maybe (fromJust)

import qualified HaskellReact.BackboneRouter as BR
import HaskellReact hiding (id)
import Moment (now, requireMoment, day)

import qualified Crm.Shared.Machine as M
import qualified Crm.Shared.MachineType as MT
import qualified Crm.Shared.MachineKind as MK
import qualified Crm.Shared.UpkeepMachine as UM
import qualified Crm.Shared.Upkeep as U
import qualified Crm.Shared.UpkeepSequence as US
import qualified Crm.Shared.Company as C
import qualified Crm.Shared.ContactPerson as CP
import qualified Crm.Shared.YearMonthDay as YMD
import qualified Crm.Shared.Direction as DIR
import qualified Crm.Shared.Employee as E
import qualified Crm.Shared.ExtraField as EF

import Crm.Data.MachineData (MachineData(MachineData), MachineNew(MachineNew)
  , MachineDetail(MachineDetail))
import qualified Crm.Data.Data as D
import qualified Crm.Data.UpkeepData as UD
import qualified Crm.Data.EmployeeData as ED
import Crm.Server (fetchMachine, fetchPlannedUpkeeps, fetchFrontPageData, fetchEmployees,
  fetchCompany, fetchUpkeeps, fetchUpkeep, fetchMachineTypes, fetchMachineTypeById, fetchExtraFieldSettings,
  fetchMachinePhotos, fetchEmployee, fetchContactPersons, fetchContactPerson, fetchCompaniesForMap, fetchMachinesInCompany)
import Crm.Helpers (parseSafely, showCompanyId, displayDate)
import qualified Crm.Validation as V

newtype CrmRouter = CrmRouter BR.BackboneRouter
newtype CrmRoute = CrmRoute Text

dashboard :: CrmRoute
dashboard = CrmRoute "dashboard"

defaultFrontPage :: CrmRoute
defaultFrontPage = frontPage C.NextService DIR.Asc

frontPage :: C.OrderType -> DIR.Direction -> CrmRoute
frontPage order direction = CrmRoute $ "home/" <> (case order of
  C.CompanyName -> "CompanyName"
  _ -> "NextService") <> "/" <> (case direction of
  DIR.Asc -> "Asc"
  DIR.Desc -> "Desc")

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

newContactPerson :: C.CompanyId -> CrmRoute
newContactPerson companyId = CrmRoute $ "companies/" <> showCompanyId companyId <> "/new-contact-person"

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

employeePage :: CrmRoute
employeePage = CrmRoute "employees"

newEmployee :: CrmRoute
newEmployee = CrmRoute "employees/new"

editEmployee :: E.EmployeeId -> CrmRoute
editEmployee employeeId = CrmRoute $ "employees/" <> (showInt $ E.getEmployeeId employeeId)

contactPersonList :: C.CompanyId -> CrmRoute
contactPersonList companyId = CrmRoute $ "companies/" <> (showInt $ C.getCompanyId companyId) <> "/contact-persons"

contactPersonEdit :: CP.ContactPersonId -> CrmRoute
contactPersonEdit contactPersonId = CrmRoute $ "contact-persons/" <> (showInt $ CP.getContactPersonId contactPersonId)

extraFields :: CrmRoute
extraFields = CrmRoute $ "extra-fields"

startRouter :: Var D.AppState -> Fay CrmRouter
startRouter appVar = let
  modify' newState = modify appVar (\appState -> appState { D.navigation = newState })
  withCompany :: [Text]
              -> (C.CompanyId -> (C.Company, [(M.MachineId, M.Machine,
                   C.CompanyId, MT.MachineTypeId, MT.MachineType, Maybe CP.ContactPerson)]) 
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
    "dashboard", const $
      fetchCompaniesForMap (\companiesTriple -> 
        modify appVar (\appState -> appState { D.navigation = D.Dashboard companiesTriple }))
  ),(
    "", const $
      fetchFrontPageData C.NextService DIR.Asc (\data' -> modify appVar 
        (\appState -> appState { D.navigation = D.FrontPage (C.NextService, DIR.Asc) data' }))
  ),(
    "home/:order/:direction", \params -> let
      firstParam = head params
      secondParam = head $ tail params
      order = if firstParam == "CompanyName"
        then C.CompanyName
        else C.NextService
      direction = if secondParam == "Asc"
        then DIR.Asc
        else DIR.Desc
      in fetchFrontPageData order direction (\data' ->
        modify appVar (\appState -> appState { D.navigation = 
          D.FrontPage (order, direction) data' }))
  ),(
    "extra-fields", const $ fetchExtraFieldSettings $ \list -> let
      makeIdsAssigned = map (\(fId, field) -> (EF.Assigned fId, field)) 
      withAssignedIds = map (\(enum,fields) -> (enum, makeIdsAssigned fields)) list
      in modify' $ D.ExtraFields MK.Compressor withAssignedIds
  ),(
    "companies/:id", \params -> let
      cId = head params
      in case (parseSafely cId, cId) of
        (Just(cId''), _) -> let
          companyId = C.CompanyId cId''
          in fetchCompany companyId (\(company,machines) -> 
            modify appVar (\appState -> appState {
              D.navigation = D.CompanyDetail companyId company False machines }))
        (_, new) | new == "new" -> modify appVar (\appState -> appState {
          D.navigation = D.CompanyNew C.newCompany })
        _ -> modify' D.NotFound
  ),(
    "companies/:id/new-machine-phase1", \params ->
    withCompany
      params
      (\companyId (_,_) ->
        D.MachineNewPhase1 Nothing (MT.newMachineType,[]) companyId)
  ),(
    "companies/:id/new-contact-person", \params ->
    withCompany
      params
      (\companyId (_,_) ->
        D.ContactPersonPage CP.newContactPerson Nothing companyId)
  ),(
    "companies/:id/new-machine-phase2", \params -> let
      cId = head params
      in case (parseSafely cId) of
        Just companyIdInt -> do
          appState <- get appVar
          let
            machineTypeTuple = D.machineTypeFromPhase1 appState
            machineKind = MT.kind $ fst machineTypeTuple
            maybeMachineTypeId = D.maybeMachineIdFromPhase1 appState
            companyId = C.CompanyId companyIdInt
            machine' = (M.newMachine nowYMD)
            machine = case machineKind of
              MK.Compressor -> machine'
              MK.Dryer -> machine' { M.mileagePerYear = 8760 }
            machineQuadruple = (machine, "")
          fetchContactPersons companyId $ \cps -> fetchMachinesInCompany companyId $ \otherMachines -> modify' $ 
            D.MachineScreen $ MachineData machineQuadruple machineKind machineTypeTuple
              (nowYMD, False) Nothing cps V.new Nothing otherMachines (Right $ MachineNew companyId maybeMachineTypeId)
        _ -> modify' D.NotFound
  ),(
    "companies/:id/new-maintenance", \params ->
      fetchEmployees (\employees -> 
        withCompany
          params
          (\companyId (_, machines') -> let
            machines = map (\(a,b,c,d,e,_) -> (a,b,c,d,e)) machines'
            notCheckedUpkeepMachines = map (\(machineId,_,_,_,_) -> 
              (UM.newUpkeepMachine, machineId)) machines
            in D.UpkeepScreen $ UD.UpkeepData (U.newUpkeep nowYMD, []) 
              machines notCheckedUpkeepMachines
              ((nowYMD, False), displayDate nowYMD) employees 
              Nothing V.new (Right $ UD.UpkeepNew $ Left companyId)))
  ),(
    "companies/:id/contact-persons", \params ->
      case (parseSafely $ head params) of
        Just(companyIdInt) -> 
          let companyId = C.CompanyId companyIdInt
          in fetchContactPersons companyId (\data' -> let
            ns = D.ContactPersonList data'
            in modify' ns)
        _ -> modify' D.NotFound
  ),(
    "companies/:id/maintenances", \params ->
      case (parseSafely $ head params) of
        Just(companyIdInt) -> 
          let companyId = C.CompanyId companyIdInt
          in fetchUpkeeps companyId (\data' -> let
            ns = D.UpkeepHistory data' companyId
            in modify' ns)
        _ -> modify' D.NotFound
  ),(
    "machines/:id", \params -> let
      maybeId = parseSafely $ head params
      in case maybeId of
        Just(machineId') -> let
          machineId = M.MachineId machineId'
          in fetchMachine machineId
            (\(companyId, machine, machineTypeId, machineTypeTuple, 
                machineNextService, contactPersonId, upkeeps, otherMachineId, machineSpecificData) ->
              fetchMachinePhotos machineId $ \photos ->
                let 
                  machineQuadruple = (machine, "")
                  startDateInCalendar = maybe nowYMD id (M.machineOperationStartDate machine)
                in fetchContactPersons companyId $ \cps -> fetchMachinesInCompany companyId $ \otherMachines ->
                  modify' $ D.MachineScreen $ MachineData
                    machineQuadruple machineSpecificData machineTypeTuple (startDateInCalendar, False) 
                      contactPersonId cps V.new otherMachineId otherMachines
                        (Left $ MachineDetail machineId machineNextService False machineTypeId photos upkeeps companyId))
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
              in modify' $ D.UpkeepScreen $ UD.UpkeepData (upkeep', upkeepMachines) machines
                (notCheckedMachines' machines upkeepMachines) ((upkeepDate, False), displayDate upkeepDate) employees 
                selectedEmployee V.new (Left $ UD.UpkeepClose upkeepId companyId)))
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
            let upkeepSequences' = map ((\us -> (us, showInt $ US.repetition us ))) upkeepSequences
            in modify' $ D.MachineTypeEdit machineTypeId (machineType, upkeepSequences')) . fromJust)
        _ -> modify' D.NotFound 
  ),(
    "upkeeps/:id/replan", \params -> let
      upkeepId'' = parseSafely $ head params
      in case upkeepId'' of
        Just (upkeepId') -> let
          upkeepId = U.UpkeepId upkeepId'
          in fetchUpkeep upkeepId (\(_, (upkeep, employeeId, upkeepMachines), machines) ->
            fetchEmployees (\employees ->
              modify' $ D.UpkeepScreen $ UD.UpkeepData (upkeep, upkeepMachines) machines
                (notCheckedMachines' machines upkeepMachines) ((U.upkeepDate upkeep, False), displayDate $ U.upkeepDate upkeep)
                employees employeeId V.new (Right $ UD.UpkeepNew $ Right upkeepId)))
        _ -> modify' D.NotFound
  ),(
    "contact-persons/:id", \params -> let
      maybeId = parseSafely $ head params
      in case maybeId of 
        Just (contactPersonId') -> let
          contactPersonId = CP.ContactPersonId contactPersonId'
          in fetchContactPerson contactPersonId (\(cp, companyId) -> modify' $ D.ContactPersonPage cp (Just contactPersonId) companyId)
        _ -> modify' D.NotFound
  ),(
    "employees", const $
      fetchEmployees (\employees -> modify' $ D.EmployeeList employees)
  ),(
    "employees/:id", \params -> let
      headParam = head params
      in case parseSafely headParam of
        Just employeeId' -> let
          employeeId = E.EmployeeId employeeId'
          in fetchEmployee employeeId (\employee ->
            modify' $ D.EmployeeManage $ ED.EmployeeData employee (Just employeeId))
        Nothing | headParam == "new" -> modify' $ D.EmployeeManage $ ED.EmployeeData E.newEmployee Nothing
        Nothing -> modify' D.NotFound )]

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
     => a
     -> CrmRoute
     -> CrmRouter
     -> DOMElement
link children (CrmRoute route) (CrmRouter router) = 
  BR.link children route router
