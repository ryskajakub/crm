module Crm.Server (
  createCompany , 
  createMachine , 
  createUpkeep , 
  createEmployee ,
  createContactPerson ,

  updateUpkeep ,
  updateMachine , 
  updateCompany ,
  updateContactPerson ,
  updateEmployee ,
  updateMachineType , 

  saveExtraFieldSettings ,

  uploadPhotoData ,
  uploadPhotoMeta ,

  fetchUpkeepData ,
  fetchExtraFieldSettings ,
  fetchMachine , 
  fetchMachinePhotos ,
  fetchMachinesInCompany ,
  fetchUpkeeps , 
  fetchPlannedUpkeeps , 
  fetchFrontPageData , 
  fetchMachineType ,
  fetchMachineTypeById ,
  fetchMachineTypes ,
  fetchMachineTypesAutocomplete ,
  fetchMachineTypesManufacturer ,
  fetchUpkeep ,
  fetchEmployees ,
  fetchEmployee ,
  fetchCompany ,
  fetchContactPersons ,
  fetchContactPerson ,
  fetchCompaniesForMap ,
  fetchPhoto ,
  fetchDailyPlanData ,
  fetchDailyPlanEmployees ,
  fetchEmployeeTasks ,
  fetchEmployeeTask ,

  deleteUpkeep ,
  deleteCompany ,
  deleteMachine ,
  deletePhoto ,
  deleteContactPerson ,

  testEmployeesPage ,
  status ) where

import           FFI                                 (ffi, Defined(Defined))
import           Prelude                             hiding (putStrLn)
import           Data.Text                           (Text, unpack, pack, (<>))

import qualified JQuery                              as JQ

import qualified Crm.Shared.Company                  as C
import qualified Crm.Shared.ContactPerson            as CP
import qualified Crm.Shared.Upkeep                   as U
import qualified Crm.Shared.Machine                  as M
import qualified Crm.Shared.MachineType              as MT
import qualified Crm.Shared.MachineKind              as MK
import qualified Crm.Shared.UpkeepMachine            as UM
import qualified Crm.Shared.Api                      as A
import qualified Crm.Shared.Photo                    as P
import qualified Crm.Shared.PhotoMeta                as PM
import qualified Crm.Shared.YearMonthDay             as YMD
import qualified Crm.Shared.Employee                 as E
import qualified Crm.Shared.EmployeeTask             as ET
import qualified Crm.Shared.UpkeepSequence           as US
import qualified Crm.Shared.Direction                as DIR
import qualified Crm.Shared.ExtraField               as EF
import qualified Crm.Shared.ServerRender             as SR
import           Crm.Shared.MyMaybe

import qualified Crm.Client.Employees                as XE
import qualified Crm.Client.Companies                as XC
import qualified Crm.Client.Upkeeps                  as XU
import qualified Crm.Client.Machines                 as XM
import qualified Crm.Client.Photos                   as XP
import qualified Crm.Client.PhotoMeta                as XPM
import qualified Crm.Client.MachineTypes             as XMT
import qualified Crm.Client.ContactPersons           as XCP
import qualified Crm.Client.MachineKind              as XMK
import qualified Crm.Client.Companies.Machines       as XCM
import qualified Crm.Client.Companies.ContactPersons as XCCP
import qualified Crm.Client.Companies.Upkeeps        as XCU
import qualified Crm.Client.Machines.Photos          as XMP
import qualified Crm.Client.Employees.Upkeeps        as XEU
import qualified Crm.Client.Print                    as XPP

import           Crm.Runtime
import           Crm.Helpers                         (File, encodeURIComponent, displayDateNumeral)
import qualified Crm.Router                          as R


-- helpers

status :: JQ.JQXHR -> Int
status = ffi " %1['status'] "


maxCount :: [(String, String)]
maxCount = [("count", "1000")]

-- deletions

deleteCompany :: C.CompanyId
              -> Fay ()
              -> R.CrmRouter
              -> Fay ()
deleteCompany ident cb = XC.remove ident (const cb)

deleteUpkeep :: U.UpkeepId
             -> Fay ()
              -> R.CrmRouter
             -> Fay ()
deleteUpkeep ident cb = XU.remove ident $ const cb

deleteMachine :: M.MachineId
              -> Fay ()
              -> R.CrmRouter
              -> Fay ()
deleteMachine ident cb = XM.remove ident $ const cb

deletePhoto :: P.PhotoId
            -> Fay ()
              -> R.CrmRouter
            -> Fay ()
deletePhoto ident cb = XP.remove ident $ const cb

deleteContactPerson :: CP.ContactPersonId
                    -> Fay ()
              -> R.CrmRouter
                    -> Fay ()
deleteContactPerson ident cb = XCP.remove ident $ const cb


-- fetching of data from server

dayParam :: YMD.YearMonthDay -> [(String, String)]
dayParam day = [("day", unpack . displayDateNumeral $ day)]

fetchEmployeeTasks :: 
  E.EmployeeId ->
  ((E.Employee, [(ET.EmployeeTaskId, ET.EmployeeTask)]) -> Fay ()) ->
  R.CrmRouter ->
  Fay ()
fetchEmployeeTasks employeeId callback = \r -> let
  employee = E.Employee (pack "Pepa") undefined undefined undefined
  tasks = [
    (ET.EmployeeTaskId 1, ET.EmployeeTask (YMD.YearMonthDay 2015 1 1 YMD.DayPrecision) (pack "hotovo")), 
    (ET.EmployeeTaskId 2, ET.EmployeeTask (YMD.YearMonthDay 2015 11 1 YMD.DayPrecision) (pack "konec borec"))]
  in callback (employee, tasks)

fetchEmployeeTask ::
  ET.EmployeeTaskId ->
  (ET.EmployeeTask -> Fay ()) ->
  R.CrmRouter ->
  Fay ()
fetchEmployeeTask employeeTaskId callback = \r -> let
  task = ET.EmployeeTask (YMD.YearMonthDay 2016 1 1 YMD.DayPrecision) (pack "task description")
  in callback task

fetchDailyPlanEmployees :: YMD.YearMonthDay
                        -> ([E.Employee'] -> Fay ())
              -> R.CrmRouter
                        -> Fay ()
fetchDailyPlanEmployees day = XPP.list (maxCount ++ dayParam day)

fetchDailyPlanData :: YMD.YearMonthDay
                   -> Maybe E.EmployeeId
                   -> ([(U.Upkeep, C.Company, [E.Employee'], [(M.Machine, MT.MachineType, 
                      Maybe CP.ContactPerson, (UM.UpkeepMachine, Maybe [SR.Markup]))])] -> Fay ())
              -> R.CrmRouter
                   -> Fay ()
fetchDailyPlanData day employeeId cb = remoteCall $ cb . map (\(a,b,c,d) -> (a,b,c,map
  (\(a1,a2,a3,(a4',a4'')) -> (a1,a2,toMaybe a3,(a4',toMaybe a4''))) d))
  where
    allParams = maxCount ++ dayParam day
    remoteCall = maybe (XU.listPrint allParams) (XEU.listPrint allParams) employeeId

fetchPhoto :: P.PhotoId
           -> Text
fetchPhoto photoId = apiRoot <> (pack $ A.photos ++ "/" ++ (show $ P.getPhotoId photoId))

fetchMachineTypesManufacturer :: Text -- ^ the string user typed
                              -> ([Text] -> Fay ()) -- callback filled with option that the user can pick
              -> R.CrmRouter
                              -> Fay ()
fetchMachineTypesManufacturer text = 
  XMT.listByAutocompleteManufacturer
    maxCount
    (unpack . encodeURIComponent $ text)
  
fetchMachineTypesAutocomplete :: Text -- ^ the string user typed
                              -> ([Text] -> Fay ()) -- callback filled with option that the user can pick
              -> R.CrmRouter
                              -> Fay ()
fetchMachineTypesAutocomplete text = 
  XMT.listByAutocomplete 
    maxCount
    (unpack . encodeURIComponent $ text)

fetchMachineTypes :: ([(MT.MachineType', Int)] -> Fay ()) 
              -> R.CrmRouter
  -> Fay ()
fetchMachineTypes = XMT.list maxCount

fetchMachineTypeById :: MT.MachineTypeId
                     -> (Maybe (MT.MachineTypeId, MT.MachineType, [US.UpkeepSequence]) -> Fay ())
              -> R.CrmRouter
                     -> Fay ()
fetchMachineTypeById mtId callback = 
  XMT.byById mtId (callback . toMaybe)

fetchMachineType :: Text -- ^ machine type exact match
                 -> (Maybe (MT.MachineTypeId, MT.MachineType, [US.UpkeepSequence]) -> Fay ()) -- ^ callback
              -> R.CrmRouter
                 -> Fay ()
fetchMachineType machineTypeName callback = 
  XMT.byByName 
    (unpack . encodeURIComponent $ machineTypeName)
    (callback . toMaybe)

fetchEmployees :: ([E.Employee'] -> Fay ())
              -> R.CrmRouter
               -> Fay ()
fetchEmployees = XE.list maxCount

fetchUpkeep :: U.UpkeepId -- ^ upkeep id
            -> ((C.CompanyId, (U.Upkeep, [UM.UpkeepMachine'], [E.EmployeeId]), 
               [(M.MachineId, M.Machine, MT.MachineType, US.UpkeepSequence)]) -> Fay ()) 
              -> R.CrmRouter
            -> Fay ()
fetchUpkeep = XU.bySingle 

fetchUpkeepData :: C.CompanyId
                -> ([(M.MachineId, M.Machine, MT.MachineType, US.UpkeepSequence)] -> Fay ())
              -> R.CrmRouter
                -> Fay ()
fetchUpkeepData companyId = XCU.bySingle companyId "()"

fetchUpkeeps :: C.CompanyId -- ^ company id
             -> ([(U.UpkeepId, U.Upkeep, [(UM.UpkeepMachine, MT.MachineType, M.MachineId)], [E.Employee'])] -> Fay ()) -- ^ callback
              -> R.CrmRouter
             -> Fay ()
fetchUpkeeps = XCU.list maxCount
  
fetchMachinePhotos :: M.MachineId
                   -> ([(P.PhotoId, PM.PhotoMeta)] -> Fay ())
              -> R.CrmRouter
                   -> Fay ()
fetchMachinePhotos = XMP.list maxCount

fetchMachinesInCompany :: C.CompanyId
                       -> ([(M.MachineId, M.Machine)] -> Fay ())
              -> R.CrmRouter
                       -> Fay ()
fetchMachinesInCompany = XCM.list maxCount

fetchExtraFieldSettings :: ([(MK.MachineKindEnum, [(EF.ExtraFieldId, MK.MachineKindSpecific)])] -> Fay ())
              -> R.CrmRouter
                        -> Fay ()
fetchExtraFieldSettings = XMK.byString "()"

fetchMachine :: M.MachineId -- ^ machine id
             -> ((C.CompanyId, M.Machine, MT.MachineTypeId,
                (MT.MachineType, [US.UpkeepSequence]), YMD.YearMonthDay, Maybe CP.ContactPersonId,
                [(U.UpkeepId, U.Upkeep, UM.UpkeepMachine)], Maybe M.MachineId, 
                MK.MachineKindEnum, [(EF.ExtraFieldId, MK.MachineKindSpecific, Text)]) -> Fay()) -- ^ callback
              -> R.CrmRouter
             -> Fay ()
fetchMachine machineId callback =  
  XM.byMachineId 
    machineId
    (let
      fun ((a,b,c,d),(e,e1,g,g2,f,l)) = (a,b,c,d,e,toMaybe e1,g,toMaybe g2,f,l)
      in callback . fun)

fetchEmployee :: E.EmployeeId
              -> (E.Employee -> Fay ())
              -> R.CrmRouter
              -> Fay ()
fetchEmployee = XE.byEmployeeId

fetchContactPerson :: CP.ContactPersonId
                   -> ((CP.ContactPerson, C.CompanyId) -> Fay ())
              -> R.CrmRouter
                   -> Fay ()
fetchContactPerson = XCP.byContactPersonId

fetchContactPersons :: C.CompanyId
                    -> ([(CP.ContactPersonId, CP.ContactPerson)] -> Fay ())
              -> R.CrmRouter
                    -> Fay ()
fetchContactPersons = XCCP.list maxCount

fetchCompany :: C.CompanyId -- ^ company id
             -> ((C.Company, [CP.ContactPerson'], [(M.MachineId, M.Machine, C.CompanyId, MT.MachineTypeId, 
                MT.MachineType, Maybe CP.ContactPerson, Maybe M.MachineId, YMD.YearMonthDay)]) -> Fay ()) -- ^ callback
              -> R.CrmRouter
             -> Fay ()
fetchCompany companyId callback = 
  XC.bySingle
    companyId
    (callback . (\(a0, a1, a2) -> 
      (a0, a1, (map (\((a,b,c,d,e,f,g),h) -> (a,b,c,d,e,toMaybe f,toMaybe g,h))) a2)))

fetchFrontPageData :: C.OrderType
                   -> DIR.Direction
                   -> ([(C.CompanyId, C.Company, Maybe YMD.YearMonthDay)] -> Fay ())
                   -> R.CrmRouter
                   -> Fay ()
fetchFrontPageData order direction callback router = 
  let
    lMb [] = []
    lMb ((a,b,x) : xs) = (a,b,toMaybe x) : lMb xs
  in passwordAjax
    (pack $ A.companies ++ "?count=1000&order=" ++ (case order of
      C.CompanyName -> "CompanyName"
      C.NextService -> "NextService") ++ "&direction=" ++ (case direction of
      DIR.Asc -> "Asc"
      DIR.Desc -> "Desc"))
    (callback . lMb . items)
    Nothing
    get
    Nothing
    Nothing
    router

fetchPlannedUpkeeps :: ([(U.UpkeepId, U.Upkeep, C.CompanyId, C.Company, [(M.MachineId, Text, Text)], 
                         [(E.EmployeeId, E.Employee)])] -> Fay ())
              -> R.CrmRouter
                    -> Fay ()
fetchPlannedUpkeeps = XU.listPlanned maxCount

fetchCompaniesForMap :: ([(C.CompanyId, C.Company, Maybe YMD.YearMonthDay, Maybe C.Coordinates)] -> Fay ())
              -> R.CrmRouter
                     -> Fay ()
fetchCompaniesForMap callback = 
  XC.listMap 
    maxCount
    (callback . (map (\(a,b,c,d) -> (a,b,toMaybe c,toMaybe d))))


-- creations

createCompany :: C.Company
              -> Maybe C.Coordinates
              -> (C.CompanyId -> Fay ())
              -> R.CrmRouter
              -> Fay ()
createCompany company coordinates = XC.create (company, toMyMaybe coordinates) 

createMachine :: M.Machine 
              -> C.CompanyId
              -> MT.MyEither
              -> Maybe M.ContactPersonForMachine
              -> Maybe M.MachineId
              -> [(EF.ExtraFieldId, Text)]
              -> Fay ()
              -> R.CrmRouter
              -> Fay ()
createMachine machine companyId machineType contactPersonId linkedMachineId extraFields callback = 
  XCM.create 
    companyId 
    (machine, machineType, toMyMaybe contactPersonId, toMyMaybe linkedMachineId, extraFields)
    (const callback)

createUpkeep :: (U.Upkeep, [UM.UpkeepMachine'], [E.EmployeeId])
             -> Fay ()
              -> R.CrmRouter
             -> Fay ()
createUpkeep (newUpkeep, upkeepMachines, se) callback = 
  XU.create
    (newUpkeep, upkeepMachines, se)
    (const callback)
    
createEmployee :: E.Employee
               -> Fay ()
              -> R.CrmRouter
               -> Fay ()
createEmployee employee callback =
  XE.create
    employee
    (const callback)

createContactPerson :: C.CompanyId
                    -> CP.ContactPerson
                    -> Fay ()
              -> R.CrmRouter
                    -> Fay ()
createContactPerson companyId contactPerson callback = 
  XCCP.create
    companyId
    contactPerson
    (const callback)


-- updations

updateEmployee :: E.EmployeeId
               -> E.Employee
               -> Fay ()
              -> R.CrmRouter
               -> Fay ()
updateEmployee employeeId employee callback = 
  XE.saveByEmployeeId
    employeeId
    employee
    (const callback)

updateContactPerson :: CP.ContactPersonId
                    -> CP.ContactPerson
                    -> Fay ()
              -> R.CrmRouter
                    -> Fay ()
updateContactPerson cpId cp callback =
  XCP.saveByContactPersonId
    cpId
    cp
    (const callback)

updateCompany :: C.CompanyId
              -> C.Company
              -> Maybe C.Coordinates
              -> Fay ()
              -> R.CrmRouter
              -> Fay ()
updateCompany companyId company coordinates cb = 
  XC.saveBySingle
    companyId
    (company, toMyMaybe coordinates)
    (const cb)

updateUpkeep :: U.Upkeep'
             -> [E.EmployeeId]
             -> Fay ()
              -> R.CrmRouter
             -> Fay ()
updateUpkeep (upkeepId, upkeep, upkeepMachines) employeeIds cb = 
  XU.saveBySingle
    upkeepId
    (upkeep, upkeepMachines, employeeIds)
    (const cb)

updateMachineType :: (MT.MachineTypeId, MT.MachineType, [US.UpkeepSequence])
                  -> Fay ()
              -> R.CrmRouter
                  -> Fay ()
updateMachineType (machineTypeId, machineType, upkeepSequences) cb = 
  XMT.saveByById 
    machineTypeId
    (machineType, upkeepSequences)
    (const cb)

updateMachine :: M.MachineId -- ^ machine id
              -> M.Machine
              -> Maybe M.MachineId -- ^ linked machine id
              -> Maybe CP.ContactPersonId
              -> [(EF.ExtraFieldId, Text)]
              -> Fay ()
              -> R.CrmRouter
              -> Fay ()
updateMachine machineId machine linkedMachineId contactPersonId machineSpecificData cb = 
  XM.saveByMachineId
    machineId
    (machine, toMyMaybe linkedMachineId, toMyMaybe contactPersonId, machineSpecificData)
    (const cb)


-- others

saveExtraFieldSettings :: [(MK.MachineKindEnum, [(EF.ExtraFieldIdentification, MK.MachineKindSpecific)])]
                       -> Fay ()
              -> R.CrmRouter
                       -> Fay ()
saveExtraFieldSettings data' cb = 
  XMK.saveByString "()" data' (const cb)

uploadPhotoData :: File
                -> M.MachineId
                -> (P.PhotoId -> Fay ())
                -> Fay ()
uploadPhotoData fileContents machineId callback = withPassword Nothing $ \settings ->
  JQ.ajax' $ settings {
    JQ.success = Defined callback ,
    JQ.data' = Defined fileContents ,
    JQ.url = Defined $ apiRoot <> (pack $ A.machines ++ "/" ++ (show . M.getMachineId $ machineId) ++ "/" ++ A.photos) ,
    JQ.type' = Defined post ,
    JQ.processData = Defined False ,
    JQ.contentType = Defined $ pack "application/x-www-form-urlencoded" }

uploadPhotoMeta :: PM.PhotoMeta
                -> P.PhotoId
                -> Fay ()
              -> R.CrmRouter
                -> Fay ()
uploadPhotoMeta photoMeta photoId cb = 
  XPM.saveByPhotoId photoId photoMeta (const cb)


-- | just ping the server if it works
testEmployeesPage :: Text
                  -> Fay ()
                  -> (JQ.JQXHR -> Maybe Text -> Maybe Text -> Fay ())
              -> R.CrmRouter
                  -> Fay ()
testEmployeesPage password' success error' = passwordAjax 
  (pack A.employees)
  (const success)
  Nothing
  get
  (Just error')
  (Just password')
