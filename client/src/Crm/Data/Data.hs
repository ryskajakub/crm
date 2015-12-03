module Crm.Data.Data where

import           Data.Var                  (Var, modify)
import           Data.Text                 (Text)

import qualified Crm.Shared.Company        as C
import qualified Crm.Shared.Employee       as E
import qualified Crm.Shared.ContactPerson  as CP
import qualified Crm.Shared.Machine        as M
import qualified Crm.Shared.MachineType    as MT
import qualified Crm.Shared.MachineKind    as MK
import qualified Crm.Shared.Upkeep         as U
import qualified Crm.Shared.UpkeepSequence as US
import qualified Crm.Shared.UpkeepMachine  as UM
import qualified Crm.Shared.ExtraField     as EF
import qualified Crm.Shared.YearMonthDay   as YMD
import qualified Crm.Shared.Direction      as DIR
import qualified Crm.Shared.ServerRender   as SR
import qualified Crm.Shared.Task           as T

import           Crm.Data.MachineData
import           Crm.Data.UpkeepData
import           Crm.Data.EmployeeData

import           Crm.Component.DatePicker  as DP
import           Crm.Component.Form        (InputState)

data NavigationState =
  Dashboard { companies :: [(C.CompanyId, C.Company, Maybe YMD.YearMonthDay, Maybe C.Coordinates)] } |
  FrontPage {
    ordering :: (C.OrderType, DIR.Direction) ,
    companiesNextService :: [(C.CompanyId, C.Company, Maybe YMD.YearMonthDay)] } | 
  CompanyDetail {
    companyId :: C.CompanyId , 
    company :: C.Company , 
    contactPersons :: [CP.ContactPerson'] ,
    editing :: InputState , 
    companyMachines :: [(M.MachineId, M.Machine, C.CompanyId, MT.MachineTypeId, 
      MT.MachineType, Maybe CP.ContactPerson, Maybe YMD.YearMonthDay)] ,
    lastUpkeep :: Maybe U.Upkeep } | 
  CompanyNew {
    company :: C.Company } | 
  NotFound | 
  ServerDown |
  MachineScreen MachineData |
  UpkeepScreen UpkeepData |
  UpkeepHistory {
    companyUpkeeps :: [(U.UpkeepId, U.Upkeep2Markup, [(UM.UpkeepMachineMarkup, MT.MachineType, M.MachineId)], [E.Employee'])] ,
    companyId :: C.CompanyId } |
  PlannedUpkeeps { 
    plannedUpkeeps :: [[(U.UpkeepId, U.Upkeep, C.CompanyId, C.Company, [(M.MachineId, Text, Text)], [E.Employee'])]] } |
  MachineTypeList {
    machineTypes :: [(MT.MachineType',Int)] } |
  MachineTypeEdit {
    machineTypeId :: MT.MachineTypeId ,
    machinesCount :: Int ,
    machineTypeTuple :: (MT.MachineType, [(US.UpkeepSequence, Text)]) } |
  MachineNewPhase1 {
    maybeMachineTypeId :: Maybe MT.MachineTypeId ,
    machineTypeTuple :: (MT.MachineType, [(US.UpkeepSequence, Text)]) ,
    companyId :: C.CompanyId } |
  EmployeeList {
    employees :: [(E.EmployeeId, E.Employee)] } |
  EmployeeManage EmployeeData |
  EmployeeTasksScreen EmployeeTasksData |
  EmployeeTaskScreen EmployeeTaskData |
  ContactPersonPage {
    contactPerson :: CP.ContactPerson ,
    identification :: Maybe CP.ContactPersonId ,
    companyId :: C.CompanyId } |
  ContactPersonList {
    contactPersons :: [CP.ContactPerson'] } |
  ExtraFields {
    series :: Int ,
    showSuccess :: Bool ,
    editedKind :: MK.MachineKindEnum ,
    allSettings :: [(MK.MachineKindEnum, [(EF.ExtraFieldIdentification, MK.MachineKindSpecific)])] } |
  MachinesSchema {
    machines :: [(M.MachineId, M.Machine, MT.MachineType, Maybe M.MachineId)] } |
  Login { 
    password :: Text ,
    wrongPassword :: Bool } |
  DailyPlan {
    day :: (YMD.YearMonthDay, DP.DatePickerData) ,
    employeeId :: Maybe (E.EmployeeId, [(T.TaskId, T.TaskMarkup)]) ,
    dailyPlanData :: [(U.UpkeepMarkup, C.Company, [E.Employee'], 
      [(M.Machine, MT.MachineType, Maybe CP.ContactPerson, (UM.UpkeepMachine, Maybe [SR.Markup]))])] ,
    employees :: [E.Employee'] }

data AppState = AppState {
  navigation :: NavigationState ,
  machineTypeFromPhase1 :: (MT.MachineType, [US.UpkeepSequence]) ,
  maybeMachineIdFromPhase1 :: Maybe MT.MachineTypeId }

modifyState :: Var AppState -> (NavigationState -> NavigationState) -> Fay ()
modifyState var fun = modify var (\appState' -> appState' { navigation = fun $ navigation appState' } )

defaultAppState :: AppState
defaultAppState = AppState {
  navigation = NotFound ,
  machineTypeFromPhase1 = (MT.newMachineType,[]) ,
  maybeMachineIdFromPhase1 = Nothing }
