{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Crm.Router (
  startRouter ,
  navigate ,
  link ,
  CrmRouter ,
  CrmRoute ,
  routeToText ,
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
  machinesSchema ,
  editEmployee ) where

import           Data.Text                   (fromString, showInt, Text, (<>))
import           Prelude                     hiding (div, span) 
import           Data.Var                    (Var, modify, get)
import           Data.Function               (fmap)
import           Data.Maybe                  (fromJust, onJust)

import qualified HaskellReact.BackboneRouter as BR
import           HaskellReact                hiding (id)
import           Moment                      (now, requireMoment, day)

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
import qualified Crm.Shared.ExtraField       as EF

import qualified Crm.Data.MachineData        as MD
import qualified Crm.Data.Data               as D
import qualified Crm.Data.UpkeepData         as UD
import qualified Crm.Data.EmployeeData       as ED
import           Crm.Server 
import           Crm.Helpers                 (parseSafely, showCompanyId, displayDate, rmap)
import qualified Crm.Validation              as V
import           Crm.Component.Form


newtype CrmRouter = CrmRouter BR.BackboneRouter
newtype CrmRoute = CrmRoute Text

useHandler :: (a, (b, c)) -> (c -> c') -> (b, c')
useHandler t f = (rmap f) (snd t)

routeToText :: CrmRoute -> Text
routeToText (CrmRoute r) = "/#" <> r

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

machinesSchema :: C.CompanyId -> CrmRoute
machinesSchema companyId = CrmRoute $ "companies/" <> showCompanyId companyId <> "/schema"

data Route a = Route {
  prefix :: Text ,
  postfix :: Maybe Text }

data URLEncodable a = URLEncodable {
  onIntParseFail :: Maybe (Text -> Maybe a) ,
  toURL :: a -> Text ,
  fromURL :: Int -> a }

mkSimpleURLEncodable :: (a -> Text) -> (Int -> a) -> URLEncodable a
mkSimpleURLEncodable = URLEncodable Nothing

type RouteAndMkHandler a = (a -> CrmRoute, (Text, Var D.AppState -> (a -> Fay ()) -> [Text] -> Fay ()))
prepareRouteAndMkHandler :: Route a 
                  -> URLEncodable a 
                  -> RouteAndMkHandler a
prepareRouteAndMkHandler route urlEncodable = (mkRoute, (handlerPattern, mkHandler)) where
  mkRoute routeVariable = CrmRoute $ prefix route <> "/" <> (toURL urlEncodable) routeVariable <> postfix'
  handlerPattern = prefix route <> "/:id/" <> postfix'
  mkHandler appState appStateModifier urlVariables = 
    case parsedInt of
      Just a -> appStateModifier a
      Nothing | 
        Just onIntParseFail' <- onIntParseFail urlEncodable ,
        Just routeId <- onIntParseFail' headVariable
          -> appStateModifier routeId
      Nothing -> D.modifyState appState (const D.NotFound)
      where
        headVariable = head urlVariables
        parsedInt = fromURL urlEncodable `onJust` (parseSafely headVariable)
  postfix' = maybe "" (\p -> "/" <> p) (postfix route)

companyIdEncodable :: URLEncodable C.CompanyId
companyIdEncodable = mkSimpleURLEncodable (\cId -> showInt $ C.getCompanyId cId) (C.CompanyId)

upkeepIdEncodable :: URLEncodable U.UpkeepId
upkeepIdEncodable = mkSimpleURLEncodable (\uId -> showInt $ U.getUpkeepId uId) (U.UpkeepId)

newMachinePhase1' :: RouteAndMkHandler C.CompanyId
newMachinePhase1' = prepareRouteAndMkHandler
  (Route "companies" $ Just "new-machine-phase1") companyIdEncodable

newMachinePhase2' :: RouteAndMkHandler C.CompanyId
newMachinePhase2' = prepareRouteAndMkHandler
  (Route "companies" $ Just "new-machine-phase2") companyIdEncodable

upkeepDetail' :: RouteAndMkHandler U.UpkeepId
upkeepDetail' = prepareRouteAndMkHandler
  (Route "upkeeps" $ Nothing) upkeepIdEncodable 

companyDetail' :: RouteAndMkHandler (Either Text C.CompanyId)
companyDetail' = prepareRouteAndMkHandler
  (Route "companies" $ Nothing)
  (URLEncodable 
    (Just $ \t -> if t == "new" then Just $ Left "new" else Nothing)
    (\a -> case a of Left t -> t; Right cId -> showInt . C.getCompanyId $ cId)
    (Right . C.CompanyId))

newMaintenance' :: RouteAndMkHandler C.CompanyId
newMaintenance' = prepareRouteAndMkHandler
  (Route "companies" $ Just "new-maintenance") companyIdEncodable

newContactPerson' :: RouteAndMkHandler C.CompanyId
newContactPerson' = prepareRouteAndMkHandler
  (Route "companies" $ Just "new-contact-person") companyIdEncodable

maintenances' :: RouteAndMkHandler C.CompanyId
maintenances' = prepareRouteAndMkHandler
  (Route "companies" $ Just "maintenances") companyIdEncodable

companyDetail :: C.CompanyId -> CrmRoute
companyDetail = fst companyDetail' . Right

newMachinePhase1 :: C.CompanyId -> CrmRoute
newMachinePhase1 = fst newMachinePhase1'

newMachinePhase2 :: C.CompanyId -> CrmRoute
newMachinePhase2 = fst newMachinePhase2'

newMaintenance :: C.CompanyId -> CrmRoute
newMaintenance = fst newMaintenance'

newContactPerson :: C.CompanyId -> CrmRoute
newContactPerson = fst newContactPerson'

maintenances :: C.CompanyId -> CrmRoute
maintenances = fst maintenances'

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
              -> (C.CompanyId -> (C.Company, [(M.MachineId, M.Machine, C.CompanyId, MT.MachineTypeId,
                 MT.MachineType, Maybe CP.ContactPerson, Maybe M.MachineId, YMD.YearMonthDay)]) -> D.NavigationState)
              -> Fay ()
  withCompany params newStateFun = case parseSafely $ head params of
    Just(companyId') -> let
      companyId = C.CompanyId companyId'
      in fetchCompany companyId (\data' -> let
        newState = newStateFun companyId data'
        in modify' newState )
    _ -> modify' D.NotFound 
  withCompany' :: C.CompanyId
               -> ((C.Company, [(M.MachineId, M.Machine, C.CompanyId, MT.MachineTypeId,
                  MT.MachineType, Maybe CP.ContactPerson, Maybe M.MachineId, YMD.YearMonthDay)]) -> D.NavigationState)
               -> Fay ()
  withCompany' companyId newStateFun = 
    fetchCompany companyId (\data' -> let
      newState = newStateFun data'
      in modify' newState )
  (nowYear, nowMonth, nowDay) = day $ now requireMoment
  nowYMD = YMD.YearMonthDay nowYear nowMonth nowDay YMD.DayPrecision
  in fmap CrmRouter $ BR.startRouter [
    ("dashboard", const $
      fetchCompaniesForMap (\companiesTriple -> 
        modify appVar (\appState -> appState { D.navigation = D.Dashboard companiesTriple }))) ,
    ("", const $
      fetchFrontPageData C.NextService DIR.Asc (\data' -> modify appVar 
        (\appState -> appState { D.navigation = D.FrontPage (C.NextService, DIR.Asc) data' }))) ,
    ("home/:order/:direction", \params -> let
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
          D.FrontPage (order, direction) data' }))), 
    ("extra-fields", const $ fetchExtraFieldSettings $ \list -> let
      makeIdsAssigned = map (\(fId, field) -> (EF.Assigned fId, field)) 
      withAssignedIds = map (\(enum, fields) -> (enum, makeIdsAssigned fields)) list
      in modify' $ D.ExtraFields MK.RotaryScrewCompressor withAssignedIds), 
    (useHandler companyDetail' $ \mkHandler -> mkHandler appVar $ \companyId' ->
      case companyId' of
        Left _ -> modify appVar $ \appState -> appState {
          D.navigation = D.CompanyNew C.newCompany }
        Right companyId ->
          fetchCompany companyId $ \(company,machines) -> let
            ignoreLinkage = map $ \(a,b,c,d,e,f,_,g) -> (a,b,c,d,e,f,g)
            in modify appVar $ \appState -> appState {
              D.navigation = D.CompanyDetail companyId company Display (ignoreLinkage machines)}) ,
    (useHandler newMachinePhase1' $ \mkHandler -> mkHandler appVar $ \companyId ->
      withCompany'
        companyId
        (\_ ->
          D.MachineNewPhase1 Nothing (MT.newMachineType,[]) companyId)) ,
    (useHandler newContactPerson' $ \mkHandler -> mkHandler appVar $ \companyId ->
      withCompany'
        companyId
        (\_ -> D.ContactPersonPage CP.newContactPerson Nothing companyId)) ,
    ("companies/:id/schema", \params ->
      withCompany 
        params
        $ \_ (_, machines) -> let
          pickMachines = map $ \(a,b,_,_,c,_,d,_) -> (a,b,c,d)
          in D.MachinesSchema $ pickMachines machines), 
    (useHandler newMachinePhase2' $ \mkHandler -> mkHandler appVar $ \companyId -> do
      appState <- get appVar
      let
        machineTypeTuple = D.machineTypeFromPhase1 appState
        machineKind = MT.kind $ fst machineTypeTuple
        maybeMachineTypeId = D.maybeMachineIdFromPhase1 appState
        machine' = (M.newMachine nowYMD)
        machine = case machineKind of
          MK.RotaryScrewCompressor -> machine'
          _ -> machine' { M.mileagePerYear = MK.hoursInYear }
        machineQuadruple = (machine, "")
      fetchContactPersons companyId $ \cps -> fetchMachinesInCompany companyId $ \otherMachines -> 
        fetchExtraFieldSettings $ \efSettings -> let
          extraFields' = fromJust $ lookup machineKind efSettings
          extraFieldsAdapted = (\(a,b) -> (a,b, "")) `map` extraFields'
          in modify' $ D.MachineScreen $ MD.MachineData machineQuadruple machineKind machineTypeTuple
            (nowYMD, False) Nothing cps V.new Nothing otherMachines extraFieldsAdapted 
              (Right $ MD.MachineNew companyId maybeMachineTypeId)) ,
    ("companies/:id/new-maintenance", \params ->
      fetchEmployees (\employees -> 
        withCompany
          params
          (\companyId (_, machines') -> let
            machines = map (\(a,b,c,d,e,_,_,_) -> (a,b,c,d,e)) machines'
            notCheckedUpkeepMachines = map (\(machineId,_,_,_,_) -> 
              (UM.newUpkeepMachine, machineId)) machines
            in D.UpkeepScreen $ UD.UpkeepData (U.newUpkeep nowYMD, []) 
              machines notCheckedUpkeepMachines
              ((nowYMD, False), displayDate nowYMD) employees 
              Nothing V.new (Right $ UD.UpkeepNew $ Left companyId)))) ,
    ("companies/:id/contact-persons", \params ->
      case (parseSafely $ head params) of
        Just(companyIdInt) -> 
          let companyId = C.CompanyId companyIdInt
          in fetchContactPersons companyId (\data' -> let
            ns = D.ContactPersonList data'
            in modify' ns)
        _ -> modify' D.NotFound) , 
    ("companies/:id/maintenances", \params ->
      case (parseSafely $ head params) of
        Just(companyIdInt) -> 
          let companyId = C.CompanyId companyIdInt
          in fetchUpkeeps companyId (\data' -> let
            ns = D.UpkeepHistory data' companyId
            in modify' ns)
        _ -> modify' D.NotFound) ,
    ("machines/:id", \params -> let
      maybeId = parseSafely $ head params
      in case maybeId of
        Just(machineId') -> let
          machineId = M.MachineId machineId'
          in fetchMachine machineId
            (\(companyId, machine, machineTypeId, machineTypeTuple, 
                machineNextService, contactPersonId, upkeeps, otherMachineId, machineSpecificData, extraFields') ->
              fetchMachinePhotos machineId $ \photos ->
                let 
                  machineQuadruple = (machine, "")
                  startDateInCalendar = maybe nowYMD id (M.machineOperationStartDate machine)
                in fetchContactPersons companyId $ \cps -> fetchMachinesInCompany companyId $ \otherMachines -> 
                  modify' $ D.MachineScreen $ MD.MachineData
                    machineQuadruple machineSpecificData machineTypeTuple (startDateInCalendar, False)
                      contactPersonId cps V.new otherMachineId otherMachines extraFields'
                        (Left $ MD.MachineDetail machineId machineNextService Display machineTypeId photos upkeeps companyId))
        _ -> modify' D.NotFound) ,
    ("planned", const $
      fetchPlannedUpkeeps (\plannedUpkeeps' -> let
        newNavigation = D.PlannedUpkeeps plannedUpkeeps'
        in modify appVar (\appState -> 
          appState { D.navigation = newNavigation }))) ,
    (useHandler upkeepDetail' $ \mkHandler -> mkHandler appVar $ \upkeepId -> 
      fetchUpkeep upkeepId $ \(companyId,(upkeep,selectedEmployee,upkeepMachines),machines) -> 
        fetchEmployees $ \employees -> let
          upkeep' = upkeep { U.upkeepClosed = True }
          upkeepDate = U.upkeepDate upkeep
          in modify' $ D.UpkeepScreen $ UD.UpkeepData (upkeep', upkeepMachines) machines
            (notCheckedMachines' machines upkeepMachines) ((upkeepDate, False), displayDate upkeepDate) employees 
            selectedEmployee V.new (Left $ UD.UpkeepClose upkeepId companyId)) ,
    ("other/machine-types-list", const $
      fetchMachineTypes (\result -> modify' $ D.MachineTypeList result)) ,
    ("machine-types/:id", \params -> let
      maybeId = parseSafely $ head params
      in case maybeId of
        Just (machineTypeIdInt) -> let
          machineTypeId = (MT.MachineTypeId machineTypeIdInt)
          in fetchMachineTypeById machineTypeId ((\(_,machineType, upkeepSequences) ->
            let upkeepSequences' = map ((\us -> (us, showInt $ US.repetition us ))) upkeepSequences
            in modify' $ D.MachineTypeEdit machineTypeId (machineType, upkeepSequences')) . fromJust)
        _ -> modify' D.NotFound) ,
    ("upkeeps/:id/replan", \params -> let
      upkeepId'' = parseSafely $ head params
      in case upkeepId'' of
        Just (upkeepId') -> let
          upkeepId = U.UpkeepId upkeepId'
          in fetchUpkeep upkeepId (\(_, (upkeep, employeeId, upkeepMachines), machines) ->
            fetchEmployees (\employees ->
              modify' $ D.UpkeepScreen $ UD.UpkeepData (upkeep, upkeepMachines) machines
                (notCheckedMachines' machines upkeepMachines) ((U.upkeepDate upkeep, False), displayDate $ U.upkeepDate upkeep)
                employees employeeId V.new (Right $ UD.UpkeepNew $ Right upkeepId)))
        _ -> modify' D.NotFound) ,
    ("contact-persons/:id", \params -> let
      maybeId = parseSafely $ head params
      in case maybeId of 
        Just (contactPersonId') -> let
          contactPersonId = CP.ContactPersonId contactPersonId'
          in fetchContactPerson contactPersonId (\(cp, companyId) -> modify' $ D.ContactPersonPage cp (Just contactPersonId) companyId)
        _ -> modify' D.NotFound) ,
    ("employees", const $
      fetchEmployees (\employees -> modify' $ D.EmployeeList employees)) ,
    ("employees/:id", \params -> let
      headParam = head params
      in case parseSafely headParam of
        Just employeeId' -> let
          employeeId = E.EmployeeId employeeId'
          in fetchEmployee employeeId (\employee ->
            modify' $ D.EmployeeManage $ ED.EmployeeData employee (Just employeeId))
        Nothing | headParam == "new" -> modify' $ D.EmployeeManage $ ED.EmployeeData E.newEmployee Nothing
        Nothing -> modify' D.NotFound)]

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
