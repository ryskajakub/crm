module Crm.Server (
  items ,
  passwordAjax ,
  post ,
  put ,
  delete ,
  get ,

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

  deleteUpkeep ,
  deleteCompany ,
  deleteMachine ,
  deletePhoto ,
  deleteContactPerson ,

  testEmployeesPage ,
  status ) where

import           FFI                                 (ffi, Defined(Defined))
import           Prelude                             hiding (putStrLn)
import           Data.Text                           (Text, unpack, pack)

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
import qualified Crm.Shared.UpkeepSequence           as US
import qualified Crm.Shared.Direction                as DIR
import qualified Crm.Shared.ExtraField               as EF
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

import           Crm.Runtime
import           Crm.Helpers                         (File, rmap, encodeURIComponent)
import qualified Crm.Router                          as R


-- helpers

status :: JQ.JQXHR -> Int
status = ffi " %1['status'] "


-- deletions

deleteCompany :: C.CompanyId
              -> Fay ()
              -> Fay ()
deleteCompany ident cb = XC.removeBySingle ident $ const cb

deleteUpkeep :: U.UpkeepId
             -> Fay ()
             -> Fay ()
deleteUpkeep ident cb = XU.removeBySingle ident $ const cb

deleteMachine :: M.MachineId
              -> Fay ()
              -> Fay ()
deleteMachine ident cb = XM.removeByMachineId ident $ const cb

deletePhoto :: P.PhotoId
            -> Fay ()
            -> Fay ()
deletePhoto ident cb = XP.removeByPhotoId ident $ const cb

deleteContactPerson :: CP.ContactPersonId
                    -> Fay ()
                    -> Fay ()
deleteContactPerson ident cb = XCP.removeByContactPersonId ident $ const cb


-- fetching of data from server

fetchPhoto :: P.PhotoId
           -> Text
fetchPhoto photoId = pack $ A.photos ++ "/" ++ (show $ P.getPhotoId photoId)

fetchMachineTypesManufacturer :: Text -- ^ the string user typed
                              -> ([Text] -> Fay ()) -- callback filled with option that the user can pick
                              -> Fay ()
fetchMachineTypesManufacturer text = 
  XMT.listByAutocompleteManufacturer
    (unpack . encodeURIComponent . encodeURIComponent $ text)
  
fetchMachineTypesAutocomplete :: Text -- ^ the string user typed
                              -> ([Text] -> Fay ()) -- callback filled with option that the user can pick
                              -> Fay ()
fetchMachineTypesAutocomplete text = 
  XMT.listByAutocomplete 
    (unpack . encodeURIComponent . encodeURIComponent $ text)

fetchMachineTypes :: ([(MT.MachineType', Int)] -> Fay ()) -> Fay ()
fetchMachineTypes = XMT.list

fetchMachineTypeById :: MT.MachineTypeId
                     -> (Maybe (MT.MachineTypeId, MT.MachineType, [US.UpkeepSequence]) -> Fay ())
                     -> Fay ()
fetchMachineTypeById mtId callback = 
  XMT.byById mtId (callback . toMaybe)

fetchMachineType :: Text -- ^ machine type exact match
                 -> (Maybe (MT.MachineTypeId, MT.MachineType, [US.UpkeepSequence]) -> Fay ()) -- ^ callback
                 -> Fay ()
fetchMachineType machineTypeName callback = 
  XMT.byByName 
    (unpack . encodeURIComponent . encodeURIComponent $ machineTypeName)
    (callback . toMaybe)

fetchEmployees :: ([E.Employee'] -> Fay ())
               -> Fay ()
fetchEmployees = XE.list

fetchUpkeep :: U.UpkeepId -- ^ upkeep id
            -> ((C.CompanyId, (U.Upkeep, Maybe E.EmployeeId, [UM.UpkeepMachine']), [(M.MachineId, M.Machine, 
                 C.CompanyId, MT.MachineTypeId, MT.MachineType)]) -> Fay ()) 
            -> Fay ()
fetchUpkeep upkeepId callback = 
  XU.bySingle upkeepId 
  (callback . (\(a,(u,u2,u3),b) -> (a,(u,toMaybe u2,u3),b)))

fetchUpkeeps :: C.CompanyId -- ^ company id
             -> ([(U.UpkeepId, U.Upkeep, [(UM.UpkeepMachine, MT.MachineType, M.MachineId)], 
                Maybe E.Employee')] -> Fay ()) -- ^ callback
             -> Fay ()
fetchUpkeeps companyId callback = 
  XCU.list
    companyId
    (callback . (map (\(a,b,c,employee) -> (a,b,c,toMaybe employee))))
  
fetchMachinePhotos :: M.MachineId
                   -> ([(P.PhotoId, PM.PhotoMeta)] -> Fay ())
                   -> Fay ()
fetchMachinePhotos = XMP.list

fetchMachinesInCompany :: C.CompanyId
                       -> ([(M.MachineId, M.Machine)] -> Fay ())
                       -> Fay ()
fetchMachinesInCompany = XCM.list 

fetchExtraFieldSettings :: ([(MK.MachineKindEnum, [(EF.ExtraFieldId, MK.MachineKindSpecific)])] -> Fay ())
                        -> Fay ()
fetchExtraFieldSettings = XMK.byString "()"

fetchMachine :: M.MachineId -- ^ machine id
             -> ((C.CompanyId, M.Machine, MT.MachineTypeId,
                (MT.MachineType, [US.UpkeepSequence]), YMD.YearMonthDay, Maybe CP.ContactPersonId,
                [(U.UpkeepId, U.Upkeep, UM.UpkeepMachine, Maybe E.Employee)], Maybe M.MachineId, 
                MK.MachineKindEnum, [(EF.ExtraFieldId, MK.MachineKindSpecific, Text)]) -> Fay()) -- ^ callback
             -> Fay ()
fetchMachine machineId callback =  
  XM.byMachineId 
    machineId
    (let
      fun2 (a,b,c,d) = (a,b,c,toMaybe d)
      fun ((a,b,c,d),(e,e1,g,g2,f,l)) = (a,b,c,d,e,toMaybe e1,map fun2 g,toMaybe g2,f,l)
      in callback . fun)

fetchEmployee :: E.EmployeeId
              -> (E.Employee -> Fay ())
              -> Fay ()
fetchEmployee = XE.byEmployeeId

fetchContactPerson :: CP.ContactPersonId
                   -> ((CP.ContactPerson, C.CompanyId) -> Fay ())
                   -> Fay ()
fetchContactPerson = XCP.byContactPersonId

fetchContactPersons :: C.CompanyId
                    -> ([(CP.ContactPersonId, CP.ContactPerson)] -> Fay ())
                    -> Fay ()
fetchContactPersons = XCCP.list

fetchCompany :: C.CompanyId -- ^ company id
             -> ((C.Company, [(M.MachineId, M.Machine, C.CompanyId, MT.MachineTypeId, 
                MT.MachineType, Maybe CP.ContactPerson, Maybe M.MachineId, YMD.YearMonthDay)]) -> Fay ()) -- ^ callback
             -> Fay ()
fetchCompany companyId callback = 
  XC.bySingle
    companyId
    (callback . (rmap (map (\((a,b,c,d,e,f,g),h) -> (a,b,c,d,e,toMaybe f,toMaybe g,h)))))

fetchFrontPageData :: C.OrderType
                   -> DIR.Direction
                   -> R.CrmRouter
                   -> ([(C.CompanyId, C.Company, Maybe YMD.YearMonthDay)] -> Fay ())
                   -> Fay ()
fetchFrontPageData order direction router callback = 
  let
    lMb [] = []
    lMb ((a,b,x) : xs) = (a,b,toMaybe x) : lMb xs
  in passwordAjax
    (pack $ A.companies ++ "&order=" ++ (case order of
      C.CompanyName -> "CompanyName"
      C.NextService -> "NextService") ++ "&direction=" ++ (case direction of
      DIR.Asc -> "Asc"
      DIR.Desc -> "Desc"))
    (callback . lMb . items)
    Nothing
    get
    (Just $ \jqxhr _ _ -> if status jqxhr == 401
      then R.navigate R.login router
      else return ())
    Nothing

fetchPlannedUpkeeps :: ([(U.UpkeepId, U.Upkeep, C.CompanyId, C.Company)] -> Fay ())
                    -> Fay ()
fetchPlannedUpkeeps = XU.listPlanned

fetchCompaniesForMap :: ([(C.CompanyId, C.Company, Maybe YMD.YearMonthDay, Maybe C.Coordinates)] -> Fay ())
                     -> Fay ()
fetchCompaniesForMap callback = 
  XC.listMap
    (callback . (map (\(a,b,c,d) -> (a,b,toMaybe c,toMaybe d))))


-- creations

createCompany :: C.Company
              -> Maybe C.Coordinates
              -> (C.CompanyId -> Fay ())
              -> Fay ()
createCompany company coordinates = XC.create (company, toMyMaybe coordinates) 

createMachine :: M.Machine 
              -> C.CompanyId
              -> MT.MyEither
              -> Maybe CP.ContactPersonId
              -> Maybe M.MachineId
              -> [(EF.ExtraFieldId, Text)]
              -> Fay ()
              -> Fay ()
createMachine machine companyId machineType contactPersonId linkedMachineId extraFields callback = 
  XCM.create 
    companyId 
    (machine, machineType, toMyMaybe contactPersonId, toMyMaybe linkedMachineId, extraFields)
    (const callback)

createUpkeep :: (U.Upkeep, [UM.UpkeepMachine'], Maybe E.EmployeeId)
             -> Fay ()
             -> Fay ()
createUpkeep (newUpkeep, upkeepMachines, maybeEmployeeId) callback = 
  XU.create
    (newUpkeep, upkeepMachines, toMyMaybe maybeEmployeeId)
    (const callback)
    
createEmployee :: E.Employee
               -> Fay ()
               -> Fay ()
createEmployee employee callback =
  XE.create
    employee
    (const callback)

createContactPerson :: C.CompanyId
                    -> CP.ContactPerson
                    -> Fay ()
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
               -> Fay ()
updateEmployee employeeId employee callback = 
  XE.saveByEmployeeId
    employeeId
    employee
    (const callback)

updateContactPerson :: CP.ContactPersonId
                    -> CP.ContactPerson
                    -> Fay ()
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
              -> Fay ()
updateCompany companyId company coordinates cb = 
  XC.saveBySingle
    companyId
    (company, toMyMaybe coordinates)
    (const cb)

updateUpkeep :: (U.Upkeep', Maybe E.EmployeeId)
             -> Fay ()
             -> Fay ()
updateUpkeep ((upkeepId, upkeep, upkeepMachines), maybeEmployeeId) cb = 
  XU.saveBySingle
    upkeepId
    (upkeep, upkeepMachines, toMyMaybe maybeEmployeeId)
    (const cb)

updateMachineType :: (MT.MachineTypeId, MT.MachineType, [US.UpkeepSequence])
                  -> Fay ()
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
              -> Fay ()
updateMachine machineId machine linkedMachineId contactPersonId machineSpecificData cb = 
  XM.saveByMachineId
    machineId
    (machine, toMyMaybe linkedMachineId, toMyMaybe contactPersonId, machineSpecificData)
    (const cb)


-- others

saveExtraFieldSettings :: [(MK.MachineKindEnum, [(EF.ExtraFieldIdentification, MK.MachineKindSpecific)])]
                       -> Fay ()
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
    JQ.url = Defined $ pack (A.machines ++ "/" ++ (show $ M.getMachineId machineId) ++ "/" ++ A.photos) ,
    JQ.type' = Defined post ,
    JQ.processData = Defined False ,
    JQ.contentType = Defined $ pack "application/x-www-form-urlencoded" }

uploadPhotoMeta :: PM.PhotoMeta
                -> P.PhotoId
                -> Fay ()
                -> Fay ()
uploadPhotoMeta photoMeta photoId cb = 
  XPM.saveByPhotoId photoId photoMeta (const cb)


-- | just ping the server if it works
testEmployeesPage :: Text
                  -> Fay ()
                  -> (JQ.JQXHR -> Maybe Text -> Maybe Text -> Fay ())
                  -> Fay ()
testEmployeesPage password' success error' = passwordAjax 
  (pack A.employees)
  (const success)
  Nothing
  get
  (Just error')
  (Just password')
