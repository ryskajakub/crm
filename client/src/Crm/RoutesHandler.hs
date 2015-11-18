{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Crm.RoutesHandler (
  startRouter) where

import           Data.Text                   (fromString, showInt)
import           Prelude                     hiding (div, span) 
import           Data.Var                    (Var, modify, get)
import           Data.Function               (fmap)
import           Data.Maybe                  (fromJust, onJust)

import qualified HaskellReact.BackboneRouter as BR
import qualified Moment                      as M

import qualified Crm.Shared.Machine          as M
import qualified Crm.Shared.MachineType      as MT
import qualified Crm.Shared.MachineKind      as MK
import qualified Crm.Shared.UpkeepMachine    as UM
import qualified Crm.Shared.Upkeep           as U
import qualified Crm.Shared.UpkeepSequence   as US
import qualified Crm.Shared.Company          as C
import qualified Crm.Shared.ContactPerson    as CP
import qualified Crm.Shared.YearMonthDay     as YMD
import qualified Crm.Shared.Direction        as DIR
import qualified Crm.Shared.Employee         as E
import qualified Crm.Shared.Task             as T
import qualified Crm.Shared.ExtraField       as EF

import qualified Crm.Data.MachineData        as MD
import qualified Crm.Data.Data               as D
import qualified Crm.Data.UpkeepData         as UD
import qualified Crm.Data.EmployeeData       as ED
import           Crm.Server
import           Crm.Router
import           Crm.Helpers                 (displayDate, rmap, parseSafely)
import qualified Crm.Validation              as V
import           Crm.Component.Form
import           Crm.Component.DatePicker    as DP
import           Crm.Types                   (DisplayedNote (..))

-- handler

startRouter :: Var D.AppState -> Fay CrmRouter
startRouter appVar = startedRouter where
  startedRouter = fmap CrmRouter $ BR.startRouter $ otherRoutes ++ appliedRoutes
  modify' newState = modify appVar (\appState -> appState { D.navigation = newState })
  withCompany' :: C.CompanyId
               -> ((C.Company, [CP.ContactPerson'], [(M.MachineId, M.Machine, C.CompanyId, MT.MachineTypeId,
                  MT.MachineType, Maybe CP.ContactPerson, Maybe M.MachineId, YMD.YearMonthDay)]) -> D.NavigationState)
               -> CrmRouter
               -> Fay ()
  withCompany' companyId newStateFun = 
    fetchCompany companyId $ \data' -> let
      newState = newStateFun data'
      in modify' newState
  (nowYear, nowMonth, nowDay) = M.day . M.now $ M.requireMoment
  nowYMD = YMD.YearMonthDay nowYear nowMonth nowDay YMD.DayPrecision

  appliedRoutes = map (\tuple -> rmap (\f r -> f (CrmRouter r) appVar) tuple) routes
  otherRoutes = [
    ("", \router _ -> let
      crmRouter = CrmRouter router
      in fetchFrontPageData C.NextService DIR.Asc (\data' -> modify appVar 
        $ \appState -> appState { D.navigation = D.FrontPage (C.NextService, DIR.Asc) data' }) crmRouter ) ,
    ("daily-plan/:date/employee/:employee", \router params -> let
      crmRouter = CrmRouter router
      in case M.parse M.requireMoment . head $ params of
        Just (moment) -> let
          (year, month, day) = M.day moment
          ymd = YMD.YearMonthDay year month day (YMD.DayPrecision)
          employeeId = onJust E.EmployeeId . parseSafely . head . tail $ params
          in fetchDailyPlanData ymd employeeId (\data' ->
            fetchDailyPlanEmployees ymd (\dpe -> let
              day = (ymd, DP.DatePickerData ymd False (displayDate ymd))
              modifyAppvar employeeTasks = modify appVar $ \appState -> appState { D.navigation = D.DailyPlan day employeeTasks data' dpe }
              in case employeeId of
                Just employeeId' -> fetchMarkupTasks employeeId' (\tasks -> modifyAppvar $ Just (employeeId', tasks)) crmRouter
                Nothing -> modifyAppvar Nothing
                  ) crmRouter ) crmRouter
        Nothing -> modify' D.NotFound) ,
    ("home/:order/:direction", \router params -> let
      firstParam = head params
      secondParam = head $ tail params
      order = if firstParam == "CompanyName"
        then C.CompanyName
        else C.NextService
      direction = if secondParam == "Asc"
        then DIR.Asc
        else DIR.Desc
      crmRouter = CrmRouter router
      in fetchFrontPageData order direction (\data' ->
        modify appVar $ \appState -> appState { D.navigation = 
          D.FrontPage (order, direction) data' }) crmRouter )]

  newDatePickerData = ((nowYMD, False), displayDate nowYMD)

  routes = [
    useHandler login' $ const . const $ 
      modify appVar $ \appState -> appState { D.navigation = D.Login "" False } ,
    useHandler dashboard' $ const $
      fetchCompaniesForMap $ \companiesTriple -> 
        modify appVar $ \appState -> appState { D.navigation = D.Dashboard companiesTriple } ,
    useHandler extraFields' $ const $
      fetchExtraFieldSettings $ \list -> let
        makeIdsAssigned = map (\(fId, field) -> (EF.Assigned fId, field)) 
        withAssignedIds = map (\(enum, fields) -> (enum, makeIdsAssigned fields)) list
        in modify' $ D.ExtraFields 0 False MK.RotaryScrewCompressor withAssignedIds ,
    useHandler companyDetail' $ \companyId' ->
      case companyId' of
        Left _ -> const $ modify appVar $ \appState -> appState {
          D.navigation = D.CompanyNew C.newCompany }
        Right companyId ->
          fetchCompany companyId $ \(company, contactPersons, machines) -> let
            ignoreLinkage = map $ \(a,b,c,d,e,f,_,g) -> (a,b,c,d,e,f,g)
            in modify appVar $ \appState -> appState {
              D.navigation = D.CompanyDetail 
                companyId company contactPersons Display (ignoreLinkage machines)} ,
    useHandler newMachinePhase1' $ \companyId ->
      withCompany'
        companyId
        (\_ ->
          D.MachineNewPhase1 Nothing (MT.newMachineType,[]) companyId) ,
    useHandler newContactPerson' $ \companyId ->
      withCompany'
        companyId
        (\_ -> D.ContactPersonPage CP.newContactPerson Nothing companyId) ,
    useHandler machinesSchema' $ \companyId -> 
      withCompany'
        companyId $
        \(_, _, machines) -> let
          pickMachines = map $ \(a,b,_,_,c,_,d,_) -> (a,b,c,d)
          in D.MachinesSchema $ pickMachines machines ,
    useHandler newMachinePhase2' $ \companyId router -> do
      appState <- get appVar
      let
        machineTypeTuple = D.machineTypeFromPhase1 appState
        machineKind = MT.kind $ fst machineTypeTuple
        maybeMachineTypeId = D.maybeMachineIdFromPhase1 appState
        machine' = M.newMachine' Nothing
        machine = case machineKind of
          MK.RotaryScrewCompressor -> machine'
          _ -> machine' { M.mileagePerYear = MK.hoursInYear }
        machineTuple = (machine, "", showInt . M.mileagePerYear $ machine)
      fetchContactPersons companyId (\cps -> (fetchMachinesInCompany companyId $ \otherMachines -> 
        fetchExtraFieldSettings (\efSettings -> let
          extraFields'' = fromJust $ lookup machineKind efSettings
          extraFieldsAdapted = (\(a,b) -> (a,b, "")) `map` extraFields''
          in modify' $ D.MachineScreen $ MD.MachineData machineTuple machineKind machineTypeTuple
            (nowYMD, False) Nothing cps V.new Nothing otherMachines extraFieldsAdapted 
              (Right $ MD.MachineNew companyId maybeMachineTypeId (CP.newContactPerson, MD.ById))) router ) router ) router ,
    useHandler newMaintenance' $ \companyId router -> 
      fetchUpkeepData companyId (\ud ->
        fetchEmployees (\employees -> let
          notCheckedUpkeepMachines = map (\(machineId,_,_,_) -> 
            (UM.newUpkeepMachine, machineId)) ud
          in modify' $ D.UpkeepScreen $ UD.UpkeepData (U.newUpkeep nowYMD, []) 
            ud notCheckedUpkeepMachines
            newDatePickerData employees 
            [] V.new (Right $ UD.UpkeepNew $ Left companyId)) router ) router ,
    useHandler contactPersonList' $ \companyId ->
      fetchContactPersons companyId $ \data' -> let
        ns = D.ContactPersonList data'
        in modify' ns ,
    useHandler maintenances' $ \companyId ->
      fetchUpkeeps companyId $ \upkeepsData -> let
        ns = D.UpkeepHistory upkeepsData companyId
        in modify' ns ,
    useHandler machineDetail' $ \machineId router ->
      fetchMachine machineId
        (\(companyId, machine, machineTypeId, machineTypeTuple, 
            machineNextService, contactPersonId, upkeeps, otherMachineId, machineSpecificData, extraFields'') ->
          fetchMachinePhotos machineId (\photos ->
            let 
              machineTriple = (machine, "", showInt . M.mileagePerYear $ machine)
              startDateInCalendar = maybe nowYMD id (M.machineOperationStartDate machine)
            in fetchContactPersons companyId (\cps -> fetchMachinesInCompany companyId ( \otherMachines -> 
              modify' $ D.MachineScreen $ MD.MachineData
                machineTriple machineSpecificData machineTypeTuple (startDateInCalendar, False)
                  contactPersonId cps V.new otherMachineId otherMachines extraFields''
                    (Left $ MD.MachineDetail machineId machineNextService 
                      Display machineTypeId photos upkeeps companyId) ) router ) router ) router ) router ,
    useHandler plannedUpkeeps' $ const $
      fetchPlannedUpkeeps $ \plannedUpkeeps'' -> let
        newNavigation = D.PlannedUpkeeps plannedUpkeeps''
        in modify appVar $ \appState -> 
          appState { D.navigation = newNavigation } ,
    useHandler upkeepDetail' $ \upkeepId router ->
      fetchUpkeep upkeepId ( \(companyId,(upkeep, upkeepMachines, employeeIds), machines) -> 
        fetchEmployees ( \employees -> let
          upkeep' = upkeep { U.upkeepClosed = True }
          upkeepDate = U.upkeepDate upkeep
          in modify' $ D.UpkeepScreen $ UD.UpkeepData (upkeep', upkeepMachines) machines
            (notCheckedMachines' machines upkeepMachines) ((upkeepDate, False), displayDate upkeepDate) employees 
            (map Just employeeIds) V.new (Left $ UD.UpkeepClose upkeepId companyId Note) ) router ) router ,
    useHandler machineTypesList' $ const $ 
      fetchMachineTypes $ \result -> modify' $ D.MachineTypeList result ,
    useHandler machineTypeEdit' $ \machineTypeId ->
      fetchMachineTypeById machineTypeId ((\(_,machineType, upkeepSequences) ->
        let upkeepSequences' = map ((\us -> (us, showInt $ US.repetition us ))) upkeepSequences
        in modify' $ D.MachineTypeEdit machineTypeId (machineType, upkeepSequences')) . fromJust) ,
    useHandler replanUpkeep' $ \upkeepId router ->
      fetchUpkeep upkeepId ( \(_, (upkeep, upkeepMachines, employeeIds), machines) ->
        fetchEmployees ( \employees ->
          modify' $ D.UpkeepScreen $ UD.UpkeepData (upkeep, upkeepMachines) machines
            (notCheckedMachines' machines upkeepMachines) 
            ((U.upkeepDate upkeep, False), displayDate $ U.upkeepDate upkeep)
            employees (map Just employeeIds) V.new (Right $ UD.UpkeepNew $ Right upkeepId) ) router ) router ,
    useHandler contactPersonEdit' $ \contactPersonId ->
      fetchContactPerson contactPersonId $ \(cp, companyId) -> 
        modify' $ D.ContactPersonPage cp (Just contactPersonId) companyId ,
    useHandler employees' $ const $
      fetchEmployees $ \employees -> modify' $ D.EmployeeList employees ,
    useHandler editEmployee' $ \employeeId' ->
      case employeeId' of
        Left _ -> const $ modify' $ D.EmployeeManage $ ED.EmployeeData E.newEmployee Nothing
        Right employeeId -> 
          fetchEmployee employeeId $ \employee ->
            modify' $ D.EmployeeManage $ ED.EmployeeData employee (Just employeeId) ,
    useHandler employeeTasks' $ \employeeId ->
      fetchTasks employeeId $ \employeeTasksData -> let 
        e = D.EmployeeTasksScreen $ ED.EmployeeTasksData employeeId employeeTasksData
        in modify' e ,
    useHandler newEmployeeTask' $ \employeeId ->
      const $ modify appVar $ \appState -> appState {
        D.navigation = D.EmployeeTaskScreen $ ED.EmployeeTaskData (T.newTask { T.startDate = nowYMD }) 
        (DP.DatePickerData nowYMD False (displayDate nowYMD)) (Right employeeId) } ,
    employeeTask' `useHandler` \taskId ->
      fetchTask taskId $ \task -> modify' $ D.EmployeeTaskScreen $ ED.EmployeeTaskData task 
        (DP.DatePickerData (T.startDate task) False (displayDate . T.startDate $ task)) (Left taskId) ]

notCheckedMachines' :: [(M.MachineId,t1,t2,t3)] -> [(t4,M.MachineId)] -> [(UM.UpkeepMachine, M.MachineId)]
notCheckedMachines' machines upkeepMachines = let 
  addNotCheckedMachine acc element = let 
    (machineId,_,_,_) = element
    machineChecked = find (\(_,machineId') -> 
      machineId == machineId') upkeepMachines
    in case machineChecked of
      Nothing -> (UM.newUpkeepMachine,machineId) : acc
      _ -> acc
  in foldl addNotCheckedMachine [] machines
