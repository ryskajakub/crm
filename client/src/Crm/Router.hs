{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Crm.Router (
  CrmRouter(..) ,
  CrmRoute ,

  useHandler ,
  navigate ,
  link ,
  routeToText ,

  login' ,
  dashboard' ,
  companyDetail' ,
  newMachinePhase2' ,
  newMachinePhase1' ,
  newMaintenance' ,
  replanUpkeep' ,
  maintenances' ,
  newContactPerson' ,
  plannedUpkeeps' ,
  machineTypesList' ,
  machineTypeEdit' ,
  machineDetail' ,
  contactPersonList' ,
  contactPersonEdit' ,
  extraFields' ,
  machinesSchema' ,
  upkeepDetail' ,
  editEmployee' ,
  employees' ,

  printDailyPlan ,
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
import           Data.Var                    (Var)
import           Data.Maybe                  (onJust, joinMaybe)

import qualified HaskellReact.BackboneRouter as BR
import           HaskellReact                hiding (id, p)

import qualified Crm.Shared.Machine          as M
import qualified Crm.Shared.MachineType      as MT
import qualified Crm.Shared.Upkeep           as U
import qualified Crm.Shared.Company          as C
import qualified Crm.Shared.ContactPerson    as CP
import qualified Crm.Shared.Direction        as DIR
import qualified Crm.Shared.Employee         as E

import qualified Crm.Data.Data               as D
import           Crm.Helpers                 (parseSafely, rmap)


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

printDailyPlan :: Text -> Maybe E.EmployeeId -> CrmRoute
printDailyPlan date employeeId' = CrmRoute $ "daily-plan/" <> date <> "/employee/" <> (case employeeId' of
  Just employeeId -> showInt . E.getEmployeeId $ employeeId
  Nothing -> "all")

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
