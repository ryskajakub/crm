{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Crm.Router (
  CrmRouter ,
  CrmRoute ,

  startRouter ,
  navigate ,
  link ,
  routeToText ,

  login ,
  dashboard ,
  frontPage ,
  defaultFrontPage ,
  newCompany ,
  companyDetail ,
  newMachinePhase2 ,
  newMachinePhase1 ,
  newMaintenance ,
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
  upkeepDetail ,
  editEmployee ) where

import           Data.Text                   (fromString, showInt, Text, (<>))
import           Prelude                     hiding (div, span) 
import           Data.Var                    (Var, modify, get)
import           Data.Function               (fmap)
import           Data.Maybe                  (fromJust, onJust, joinMaybe)

import qualified HaskellReact.BackboneRouter as BR
import           HaskellReact                hiding (id, p)
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
import           Crm.Helpers                 (parseSafely, displayDate, rmap)
import qualified Crm.Validation              as V
import           Crm.Component.Form

import Debug.Trace


newtype CrmRouter = CrmRouter BR.BackboneRouter
newtype CrmRoute = CrmRoute Text


-- helpers

routeToText :: CrmRoute -> Text
routeToText (CrmRoute r) = "/#" <> r

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

-- route and mk handlers orchestration

data Route a = Route {
  prefix :: Text ,
  postfix :: Maybe Text }

data URLEncodable a = URLEncodable {
  onIntParseFail :: Maybe (Text -> Maybe a) ,
  toURL :: a -> Text ,
  fromURL :: Int -> a }

type RouteAndMkHandler a = (a -> CrmRoute, (Text, (a -> Fay ()) -> Var D.AppState -> [Text] -> Fay ()))

mkSimpleURLEncodable :: (a -> Int) -> (Int -> a) -> URLEncodable a
mkSimpleURLEncodable toInt = URLEncodable Nothing (showInt . toInt)

useHandler :: (a, (b, c -> c')) -> c -> (b, c')
useHandler t c = (rmap $ \f -> f c) (snd t)

prepareRouteAndMkHandler :: Route a 
                         -> URLEncodable a 
                         -> RouteAndMkHandler a
prepareRouteAndMkHandler route urlEncodable = (mkRoute, (handlerPattern, mkHandler)) where
  mkRoute routeVariable = CrmRoute $ prefix route <> "/" <> (toURL urlEncodable) routeVariable <> postfix'
  handlerPattern = prefix route <> "/:id" <> postfix'
  mkHandler appStateModifier appState urlVariables = 
    case (parsedInt, alternativeRoute) of
      (Just a, _) -> appStateModifier a
      (Nothing, Just alternativeRouteId)  -> appStateModifier alternativeRouteId
      _ -> D.modifyState appState (const D.NotFound)
      where
        headVariable = head urlVariables
        parsedInt = fromURL urlEncodable `onJust` (parseSafely headVariable)
        alternativeRoute = joinMaybe $ (\f -> f headVariable) `onJust` onIntParseFail urlEncodable
  postfix' = maybe "" (\p -> "/" <> p) (postfix route)

prepareUnitRouteAndMkHandler :: Text
                             -> RouteAndMkHandler ()
prepareUnitRouteAndMkHandler t = (const . CrmRoute $ t, (t, mkHandler)) where
  mkHandler appStateModifier = const $ const $ appStateModifier ()


-- internal helpers

new :: Text
new = "new"

leftNew :: Either Text a
leftNew = Left "new"

mkCompaniesRoute :: Route a
mkCompaniesRoute = Route "companies" Nothing
  
mkUpkeepsRoute :: Route a
mkUpkeepsRoute = Route "upkeeps" Nothing

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


-- url encodables for id newtypes over int

companyIdEncodable :: URLEncodable C.CompanyId
companyIdEncodable = mkSimpleURLEncodable C.getCompanyId C.CompanyId

upkeepIdEncodable :: URLEncodable U.UpkeepId
upkeepIdEncodable = mkSimpleURLEncodable U.getUpkeepId U.UpkeepId

machineIdEncodable :: URLEncodable M.MachineId
machineIdEncodable = mkSimpleURLEncodable M.getMachineId M.MachineId

machineTypeIdEncodable :: URLEncodable MT.MachineTypeId
machineTypeIdEncodable = mkSimpleURLEncodable MT.getMachineTypeId MT.MachineTypeId

contactPersonIdEncodable :: URLEncodable CP.ContactPersonId
contactPersonIdEncodable = mkSimpleURLEncodable CP.getContactPersonId CP.ContactPersonId

newOrEditEncodable :: (a -> Int) -> (Int -> a) -> URLEncodable (Either Text a)
newOrEditEncodable toInt fromInt = URLEncodable
  (Just $ \t -> if t == new then Just $ Left new else Nothing)
  (\a -> case a of Left t -> t; Right cId -> showInt . toInt $ cId)
  (Right . fromInt)


-- routes and mk handlers without parameters

dashboard' :: RouteAndMkHandler ()
dashboard' = prepareUnitRouteAndMkHandler "dashboard"

extraFields' :: RouteAndMkHandler ()
extraFields' = prepareUnitRouteAndMkHandler "extra-fields"

plannedUpkeeps' :: RouteAndMkHandler ()
plannedUpkeeps' = prepareUnitRouteAndMkHandler "planned"

machineTypesList' :: RouteAndMkHandler ()
machineTypesList' = prepareUnitRouteAndMkHandler "other/machine-types-list"

employees' :: RouteAndMkHandler ()
employees' = prepareUnitRouteAndMkHandler "employees"

login' :: RouteAndMkHandler ()
login' = prepareUnitRouteAndMkHandler "login"


-- routes and mk handlers with one parameter

newMachinePhase1' :: RouteAndMkHandler C.CompanyId
newMachinePhase1' = prepareRouteAndMkHandler
  (mkCompaniesRoute { postfix = Just "new-machine-phase1" }) companyIdEncodable

newMachinePhase2' :: RouteAndMkHandler C.CompanyId
newMachinePhase2' = prepareRouteAndMkHandler
  (mkCompaniesRoute { postfix = Just "new-machine-phase2" }) companyIdEncodable

upkeepDetail' :: RouteAndMkHandler U.UpkeepId
upkeepDetail' = prepareRouteAndMkHandler
  mkUpkeepsRoute 
  upkeepIdEncodable 

companyDetail' :: RouteAndMkHandler (Either Text C.CompanyId)
companyDetail' = prepareRouteAndMkHandler
  mkCompaniesRoute
  (newOrEditEncodable C.getCompanyId C.CompanyId)

newMaintenance' :: RouteAndMkHandler C.CompanyId
newMaintenance' = prepareRouteAndMkHandler
  (mkCompaniesRoute { postfix = Just "new-maintenance" })
  companyIdEncodable

newContactPerson' :: RouteAndMkHandler C.CompanyId
newContactPerson' = prepareRouteAndMkHandler
  (mkCompaniesRoute { postfix = Just "new-contact-person" })
   companyIdEncodable

maintenances' :: RouteAndMkHandler C.CompanyId
maintenances' = prepareRouteAndMkHandler
  (mkCompaniesRoute { postfix = Just "maintenances" })
  companyIdEncodable

machinesSchema' :: RouteAndMkHandler C.CompanyId
machinesSchema' = prepareRouteAndMkHandler
  (mkCompaniesRoute { postfix = Just "schema" })
  companyIdEncodable

replanUpkeep' :: RouteAndMkHandler U.UpkeepId
replanUpkeep' = prepareRouteAndMkHandler 
  (mkUpkeepsRoute { postfix = Just "replan" })
  upkeepIdEncodable

machineDetail' :: RouteAndMkHandler M.MachineId
machineDetail' = prepareRouteAndMkHandler (Route "machines" $ Nothing) machineIdEncodable 

machineTypeEdit' :: RouteAndMkHandler MT.MachineTypeId
machineTypeEdit' = prepareRouteAndMkHandler (Route "machine-types" $ Nothing) machineTypeIdEncodable

editEmployee' :: RouteAndMkHandler (Either Text E.EmployeeId)
editEmployee' = 
  prepareRouteAndMkHandler 
  (Route "employees" $ Nothing)
  (newOrEditEncodable E.getEmployeeId E.EmployeeId)

contactPersonList' :: RouteAndMkHandler C.CompanyId
contactPersonList' = prepareRouteAndMkHandler 
  (mkCompaniesRoute { postfix = Just "contact-persons" })
  companyIdEncodable

contactPersonEdit' :: RouteAndMkHandler CP.ContactPersonId
contactPersonEdit' = prepareRouteAndMkHandler
  (Route "contact-persons" $ Nothing)
  contactPersonIdEncodable


-- routes

dashboard :: CrmRoute
dashboard = fst dashboard' ()

defaultFrontPage :: CrmRoute
defaultFrontPage = frontPage C.NextService DIR.Asc

frontPage :: C.OrderType -> DIR.Direction -> CrmRoute
frontPage order direction = CrmRoute $ "home/" <> (case order of
  C.CompanyName -> "CompanyName"
  _ -> "NextService") <> "/" <> (case direction of
  DIR.Asc -> "Asc"
  DIR.Desc -> "Desc")

login :: CrmRoute
login = fst login' ()

newCompany :: CrmRoute
newCompany = fst companyDetail' leftNew

machinesSchema :: C.CompanyId -> CrmRoute
machinesSchema = fst machinesSchema'

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
machineDetail = fst machineDetail'

plannedUpkeeps :: CrmRoute
plannedUpkeeps = fst plannedUpkeeps' ()

replanUpkeep :: U.UpkeepId -> CrmRoute
replanUpkeep = fst replanUpkeep'

upkeepDetail :: U.UpkeepId -> CrmRoute
upkeepDetail = fst upkeepDetail'

machineTypesList :: CrmRoute
machineTypesList = fst machineTypesList' ()

machineTypeEdit :: MT.MachineTypeId -> CrmRoute
machineTypeEdit = fst machineTypeEdit'

employeePage :: CrmRoute
employeePage = fst employees' ()

newEmployee :: CrmRoute
newEmployee = fst editEmployee' leftNew

editEmployee :: E.EmployeeId -> CrmRoute
editEmployee = fst editEmployee' . Right

contactPersonList :: C.CompanyId -> CrmRoute
contactPersonList = fst contactPersonList'

contactPersonEdit :: CP.ContactPersonId -> CrmRoute
contactPersonEdit = fst contactPersonEdit'

extraFields :: CrmRoute
extraFields = fst extraFields' ()


-- handler

startRouter :: Var D.AppState -> Fay CrmRouter
startRouter appVar = startedRouter where
  startedRouter = fmap CrmRouter $ BR.startRouter $ appliedRoutes ++ otherRoutes
  modify' newState = modify appVar (\appState -> appState { D.navigation = newState })
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

  appliedRoutes = map (\tuple -> rmap (\f -> f appVar) tuple) routes
  otherRoutes = [
    ("", const $
      fetchFrontPageData C.NextService DIR.Asc $ \data' -> modify appVar 
        (\appState -> appState { D.navigation = D.FrontPage (C.NextService, DIR.Asc) data' })) ,
    ("home/:order/:direction", \params -> let
      firstParam = head params
      secondParam = head $ tail params
      order = if firstParam == "CompanyName"
        then C.CompanyName
        else C.NextService
      direction = if secondParam == "Asc"
        then DIR.Asc
        else DIR.Desc
      in fetchFrontPageData order direction $ \data' ->
        modify appVar $ \appState -> appState { D.navigation = 
          D.FrontPage (order, direction) data' })]
  routes = [
    useHandler login' $ const $ 
      modify appVar $ \appState -> appState { D.navigation = D.Login "" False } ,
    useHandler dashboard' $ const $
      fetchCompaniesForMap $ \companiesTriple -> 
        modify appVar $ \appState -> appState { D.navigation = D.Dashboard companiesTriple } ,
    useHandler extraFields' $ const $
      fetchExtraFieldSettings $ \list -> let
        makeIdsAssigned = map (\(fId, field) -> (EF.Assigned fId, field)) 
        withAssignedIds = map (\(enum, fields) -> (enum, makeIdsAssigned fields)) list
        in modify' $ D.ExtraFields MK.RotaryScrewCompressor withAssignedIds ,
    useHandler companyDetail' $ \companyId' ->
      case companyId' of
        Left _ -> modify appVar $ \appState -> appState {
          D.navigation = D.CompanyNew C.newCompany }
        Right companyId ->
          fetchCompany companyId $ \(company,machines) -> let
            ignoreLinkage = map $ \(a,b,c,d,e,f,_,g) -> (a,b,c,d,e,f,g)
            in modify appVar $ \appState -> appState {
              D.navigation = D.CompanyDetail companyId company Display (ignoreLinkage machines)} ,
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
        \(_, machines) -> let
          pickMachines = map $ \(a,b,_,_,c,_,d,_) -> (a,b,c,d)
          in D.MachinesSchema $ pickMachines machines ,
    useHandler newMachinePhase2' $ \companyId -> do
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
          extraFields'' = fromJust $ lookup machineKind efSettings
          extraFieldsAdapted = (\(a,b) -> (a,b, "")) `map` extraFields''
          in modify' $ D.MachineScreen $ MD.MachineData machineQuadruple machineKind machineTypeTuple
            (nowYMD, False) Nothing cps V.new Nothing otherMachines extraFieldsAdapted 
              (Right $ MD.MachineNew companyId maybeMachineTypeId) ,
    useHandler newMaintenance' $ \companyId -> 
      fetchEmployees $ \employees -> 
        withCompany'
          companyId $
          \(_, machines') -> let
            machines = map (\(a,b,c,d,e,_,_,_) -> (a,b,c,d,e)) machines'
            notCheckedUpkeepMachines = map (\(machineId,_,_,_,_) -> 
              (UM.newUpkeepMachine, machineId)) machines
            in D.UpkeepScreen $ UD.UpkeepData (U.newUpkeep nowYMD, []) 
              machines notCheckedUpkeepMachines
              ((nowYMD, False), displayDate nowYMD) employees 
              Nothing V.new (Right $ UD.UpkeepNew $ Left companyId) ,
    useHandler newContactPerson' $ \companyId ->
      fetchContactPersons companyId $ \data' -> let
        ns = D.ContactPersonList data'
        in modify' ns ,
    useHandler maintenances' $ \companyId ->
      fetchUpkeeps companyId $ \upkeepsData -> let
        ns = D.UpkeepHistory upkeepsData companyId
        in modify' ns ,
    useHandler machineDetail' $ \machineId ->
      fetchMachine machineId
        $ \(companyId, machine, machineTypeId, machineTypeTuple, 
            machineNextService, contactPersonId, upkeeps, otherMachineId, machineSpecificData, extraFields'') ->
          fetchMachinePhotos machineId $ \photos ->
            let 
              machineDouble = (machine, "")
              startDateInCalendar = maybe nowYMD id (M.machineOperationStartDate machine)
            in fetchContactPersons companyId $ \cps -> fetchMachinesInCompany companyId $ \otherMachines -> 
              modify' $ D.MachineScreen $ MD.MachineData
                machineDouble machineSpecificData machineTypeTuple (startDateInCalendar, False)
                  contactPersonId cps V.new otherMachineId otherMachines extraFields''
                    (Left $ MD.MachineDetail machineId machineNextService 
                      Display machineTypeId photos upkeeps companyId) ,
    useHandler plannedUpkeeps' $ const $
      fetchPlannedUpkeeps $ \plannedUpkeeps'' -> let
        newNavigation = D.PlannedUpkeeps plannedUpkeeps''
        in modify appVar $ \appState -> 
          appState { D.navigation = newNavigation } ,
    useHandler upkeepDetail' $ \upkeepId -> 
      fetchUpkeep upkeepId $ \(companyId,(upkeep,selectedEmployee,upkeepMachines),machines) -> 
        fetchEmployees $ \employees -> let
          upkeep' = upkeep { U.upkeepClosed = True }
          upkeepDate = U.upkeepDate upkeep
          in modify' $ D.UpkeepScreen $ UD.UpkeepData (upkeep', upkeepMachines) machines
            (notCheckedMachines' machines upkeepMachines) ((upkeepDate, False), displayDate upkeepDate) employees 
            selectedEmployee V.new (Left $ UD.UpkeepClose upkeepId companyId) ,
    useHandler machineTypesList' $ const $ 
      fetchMachineTypes $ \result -> modify' $ D.MachineTypeList result ,
    useHandler machineTypeEdit' $ \machineTypeId ->
      fetchMachineTypeById machineTypeId ((\(_,machineType, upkeepSequences) ->
        let upkeepSequences' = map ((\us -> (us, showInt $ US.repetition us ))) upkeepSequences
        in modify' $ D.MachineTypeEdit machineTypeId (machineType, upkeepSequences')) . fromJust) ,
    useHandler replanUpkeep' $ \upkeepId ->
      fetchUpkeep upkeepId $ \(_, (upkeep, employeeId, upkeepMachines), machines) ->
        fetchEmployees $ \employees ->
          modify' $ D.UpkeepScreen $ UD.UpkeepData (upkeep, upkeepMachines) machines
            (notCheckedMachines' machines upkeepMachines) 
            ((U.upkeepDate upkeep, False), displayDate $ U.upkeepDate upkeep)
            employees employeeId V.new (Right $ UD.UpkeepNew $ Right upkeepId) ,
    useHandler contactPersonEdit' $ \contactPersonId ->
      fetchContactPerson contactPersonId $ \(cp, companyId) -> 
        modify' $ D.ContactPersonPage cp (Just contactPersonId) companyId ,
    useHandler employees' $ const $
      fetchEmployees $ \employees -> modify' $ D.EmployeeList employees ,
    useHandler editEmployee' $ \employeeId' ->
      case employeeId' of
        Left _ -> modify' $ D.EmployeeManage $ ED.EmployeeData E.newEmployee Nothing
        Right employeeId -> 
          fetchEmployee employeeId $ \employee ->
            modify' $ D.EmployeeManage $ ED.EmployeeData employee (Just employeeId) ]
