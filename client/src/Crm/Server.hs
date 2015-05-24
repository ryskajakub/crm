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

  deleteUpkeep ,
  deleteCompany ,
  deleteMachine ,
  deletePhoto ,

  getPhoto ) where

import           FFI                       (ffi, Automatic, Defined(Defined, Undefined))
import           Prelude                   hiding (putStrLn)
import           Data.Text                 (Text, (<>), unpack, pack)

import qualified JQuery                    as JQ

import qualified Crm.Shared.Company        as C
import qualified Crm.Shared.ContactPerson  as CP
import qualified Crm.Shared.Upkeep         as U
import qualified Crm.Shared.Machine        as M
import qualified Crm.Shared.MachineType    as MT
import qualified Crm.Shared.MachineKind    as MK
import qualified Crm.Shared.UpkeepMachine  as UM
import qualified Crm.Shared.Api            as A
import qualified Crm.Shared.Photo          as P
import qualified Crm.Shared.PhotoMeta      as PM
import qualified Crm.Shared.YearMonthDay   as YMD
import qualified Crm.Shared.Employee       as E
import qualified Crm.Shared.UpkeepSequence as US
import qualified Crm.Shared.Direction      as DIR
import qualified Crm.Shared.ExtraField     as EF
import           Crm.Shared.MyMaybe

import           Crm.Helpers               (File, rmap)


data Items


-- | Unthe outermost layer of the fetched list in order to get to the data
items :: Items -> Automatic a
items = ffi " %1['items'] "

noopOnError :: a -> b -> c -> Fay ()
noopOnError = (const $ const $ const $ return ())

apiRoot :: Text
apiRoot = pack "/api/v1.0.0/"

post :: Text
post = pack "POST"

put :: Text
put = pack "PUT"

delete :: Text
delete = pack "DELETE"

ajax' :: Defined a -- data to send
      -> Text -- url
      -> Text -- method PUT | POST
      -> (b -> Fay ()) -- callback
      -> Fay ()
ajax' data' url method callback = JQ.ajax' $ JQ.defaultAjaxSettings {
  JQ.success = Defined callback ,
  JQ.data' = data' ,
  JQ.url = Defined $ apiRoot <> url ,
  JQ.type' = Defined method ,
  JQ.processData = Defined False ,
  JQ.contentType = Defined $ pack "application/json" ,
  JQ.dataType = Defined $ pack "json" }

ajax :: a -- data to send
     -> Text -- url
     -> Text -- method PUT | POST
     -> (b -> Fay ()) -- callback
     -> Fay ()
ajax data' = ajax' (Defined data')

doDelete :: Text
         -> Fay ()
         -> Fay ()
doDelete url callback = ajax'
  Undefined url delete (const callback)

deleteCompany :: C.CompanyId
              -> Fay ()
              -> Fay ()
deleteCompany companyId callback = doDelete
  (pack $ A.companies ++ "/" ++ A.single ++ "/" ++ (show $ C.getCompanyId companyId))
  callback

deleteUpkeep :: U.UpkeepId
             -> Fay ()
             -> Fay ()
deleteUpkeep upkeepId callback = doDelete
  (pack $ A.upkeep ++ "/" ++ A.single ++ "/" ++ (show $ U.getUpkeepId upkeepId))
  callback

deleteMachine :: M.MachineId
              -> Fay ()
              -> Fay ()
deleteMachine machineId callback = doDelete
  (pack $ A.machines ++ "/" ++ (show $ M.getMachineId machineId))
  callback

deletePhoto :: P.PhotoId
            -> Fay ()
            -> Fay ()
deletePhoto pId callback = doDelete
  (pack $ A.photos ++ "/" ++ (show $ P.getPhotoId pId))
  callback

getPhoto :: P.PhotoId
         -> Text
getPhoto photoId = apiRoot <> (pack $ A.photos ++ "/" ++ (show $ P.getPhotoId photoId))

fetchMachineTypesManufacturer :: Text -- ^ the string user typed
                              -> ([Text] -> Fay ()) -- callback filled with option that the user can pick
                              -> Fay ()
fetchMachineTypesManufacturer text callback = do
  JQ.ajax
    (apiRoot <> (pack $ A.machineTypes ++ "/" ++ A.autocompleteManufacturer ++ "/" ++ unpack text))
    (callback . items)
    noopOnError

fetchMachineTypesAutocomplete :: Text -- ^ the string user typed
                              -> ([Text] -> Fay ()) -- callback filled with option that the user can pick
                              -> Fay ()
fetchMachineTypesAutocomplete text callback = do
  JQ.ajax
    (apiRoot <> (pack $ A.machineTypes ++ "/" ++ A.autocomplete ++ "/" ++ unpack text))
    (callback . items)
    noopOnError

fetchMachineTypes :: ([(MT.MachineType', Int)] -> Fay ()) -> Fay ()
fetchMachineTypes callback =
  JQ.ajax
    (apiRoot <> (pack $ A.machineTypes))
    (callback . items)
    noopOnError

fetchMachineTypeById :: MT.MachineTypeId
                     -> (Maybe (MT.MachineTypeId, MT.MachineType, [US.UpkeepSequence]) -> Fay ())
                     -> Fay ()
fetchMachineTypeById mtId callback = 
  JQ.ajax
    (apiRoot <> (pack (A.machineTypes ++ "/" ++ A.byId ++ "/" ++ (show $ MT.getMachineTypeId mtId))))
    (callback . toMaybe)
    noopOnError

fetchMachineType :: Text -- ^ machine type exact match
                 -> (Maybe (MT.MachineTypeId, MT.MachineType, [US.UpkeepSequence]) -> Fay ()) -- ^ callback
                 -> Fay ()
fetchMachineType machineTypeName callback = 
  JQ.ajax
    (apiRoot <> (pack $ A.machineTypes ++ "/" ++ A.byName ++ "/" ++ unpack machineTypeName))
    (callback . toMaybe)
    noopOnError

fetchEmployees :: ([E.Employee'] -> Fay ())
               -> Fay ()
fetchEmployees callback =
  JQ.ajax
    (apiRoot <> pack A.employees)
    (callback . items)
    noopOnError

fetchUpkeep :: U.UpkeepId -- ^ upkeep id
            -> ((C.CompanyId, (U.Upkeep, Maybe E.EmployeeId, [UM.UpkeepMachine']), [(M.MachineId, M.Machine, 
                 C.CompanyId, MT.MachineTypeId, MT.MachineType)]) -> Fay ()) 
            -> Fay ()
fetchUpkeep upkeepId callback =
  JQ.ajax
    (apiRoot <> (pack $ A.upkeep ++ "/" ++ A.single ++ "/" ++ (show $ U.getUpkeepId upkeepId) ++ "/"))
    (\(a,(u,u2,u3),b) -> callback(a,(u,toMaybe u2,u3),b))
    noopOnError

fetchUpkeeps :: C.CompanyId -- ^ company id
             -> ([(U.UpkeepId, U.Upkeep, [(UM.UpkeepMachine, MT.MachineType, M.MachineId)], 
                Maybe E.Employee')] -> Fay ()) -- ^ callback
             -> Fay ()
fetchUpkeeps companyId callback = 
  JQ.ajax
    (apiRoot <> (pack $ A.companies ++ "/" ++ A.single ++ "/" ++ (show $ C.getCompanyId companyId) ++ "/" ++ A.upkeep))
    (callback . (map (\(a,b,c,employee) -> (a,b,c,toMaybe employee))) . items)
    noopOnError

fetchMachinePhotos :: M.MachineId
                   -> ([(P.PhotoId, PM.PhotoMeta)] -> Fay ())
                   -> Fay ()
fetchMachinePhotos machineId callback =
  JQ.ajax
    (apiRoot <> (pack $ A.machines ++ "/" ++ (show $ M.getMachineId machineId) ++ "/" ++ A.photos))
    (callback . items)
    noopOnError

fetchMachinesInCompany :: C.CompanyId
                       -> ([(M.MachineId, M.Machine)] -> Fay ())
                       -> Fay ()
fetchMachinesInCompany companyId callback = JQ.ajax
  (apiRoot <> (pack $ A.companies ++ "/" ++ A.single ++ "/" ++ (show $ C.getCompanyId companyId) ++ "/" ++ A.machines))
  (callback . items)
  noopOnError

fetchExtraFieldSettings :: ([(MK.MachineKindEnum, [(EF.ExtraFieldId, MK.MachineKindSpecific)])] -> Fay ())
                        -> Fay ()
fetchExtraFieldSettings callback = JQ.ajax
  (apiRoot <> (pack $ A.machineKind ++ "/()"))
  callback
  noopOnError

fetchMachine :: M.MachineId -- ^ machine id
             -> ((C.CompanyId, M.Machine, MT.MachineTypeId,
                (MT.MachineType, [US.UpkeepSequence]), YMD.YearMonthDay, Maybe CP.ContactPersonId,
                [(U.UpkeepId, U.Upkeep, UM.UpkeepMachine, Maybe E.Employee)], Maybe M.MachineId, 
                MK.MachineKindEnum, [(EF.ExtraFieldId, MK.MachineKindSpecific, Text)]) -> Fay()) -- ^ callback
             -> Fay ()
fetchMachine machineId callback = let
  fun2 (a,b,c,d) = (a,b,c,toMaybe d)
  fun ((a,b,c,d),(e,e1,g,g2,f,l)) = (a,b,c,d,e,toMaybe e1,map fun2 g,toMaybe g2,f,l)
  in JQ.ajax
    (apiRoot <> (pack $ A.machines ++ "/" ++ (show $ M.getMachineId machineId)))
    (callback . fun)
    noopOnError

fetchEmployee :: E.EmployeeId
              -> (E.Employee -> Fay ())
              -> Fay ()
fetchEmployee employeeId callback = JQ.ajax
  (apiRoot <> (pack $ A.employees ++ "/" ++ (show $ E.getEmployeeId employeeId)))
  callback
  noopOnError

fetchContactPerson :: CP.ContactPersonId
                   -> ((CP.ContactPerson, C.CompanyId) -> Fay ())
                   -> Fay ()
fetchContactPerson contactPersonId callback = JQ.ajax
  (apiRoot <> (pack $ A.contactPersons ++ "/" ++ (show $ CP.getContactPersonId contactPersonId)))
  callback
  noopOnError

fetchContactPersons :: C.CompanyId
                    -> ([(CP.ContactPersonId, CP.ContactPerson)] -> Fay ())
                    -> Fay ()
fetchContactPersons companyId callback = JQ.ajax
  (apiRoot <> (pack $ A.companies ++ "/" ++ A.single ++ "/" ++ (show $ C.getCompanyId companyId) ++ "/" ++ A.contactPersons))
  (callback . items)
  noopOnError

fetchCompany :: C.CompanyId -- ^ company id
             -> ((C.Company, [(M.MachineId, M.Machine, C.CompanyId, MT.MachineTypeId, 
                MT.MachineType, Maybe CP.ContactPerson, Maybe M.MachineId, YMD.YearMonthDay)]) -> Fay ()) -- ^ callback
             -> Fay ()
fetchCompany companyId callback =
  JQ.ajax
    (apiRoot <> (pack $ A.companies ++ "/" ++ A.single ++ "/" ++ (show $ C.getCompanyId companyId)))
    (callback . (rmap (map (\((a,b,c,d,e,f,g),h) -> (a,b,c,d,e,toMaybe f,toMaybe g,h)))))
    noopOnError

fetchFrontPageData :: C.OrderType
                   -> DIR.Direction
                   -> ([(C.CompanyId, C.Company, Maybe YMD.YearMonthDay)] -> Fay ())
                   -> Fay ()
fetchFrontPageData order direction callback = let
  lMb [] = []
  lMb ((a,b,x) : xs) = (a,b,toMaybe x) : lMb xs
  in JQ.ajax
    (apiRoot <> (pack $ A.companies ++ "?order=" ++ (case order of
      C.CompanyName -> "CompanyName"
      C.NextService -> "NextService") ++ "&direction=" ++ (case direction of
      DIR.Asc -> "Asc"
      DIR.Desc -> "Desc")))
    (callback . lMb . items)
    noopOnError

fetchPlannedUpkeeps :: ([(U.UpkeepId, U.Upkeep, C.CompanyId, C.Company)] -> Fay ())
                    -> Fay ()
fetchPlannedUpkeeps callback =
  JQ.ajax
    (apiRoot <> (pack $ A.upkeep ++ "/" ++ A.planned))
    (callback . items)
    noopOnError

fetchCompaniesForMap :: ([(C.CompanyId, C.Company, Maybe YMD.YearMonthDay, Maybe C.Coordinates)] -> Fay ())
                     -> Fay ()
fetchCompaniesForMap callback =
  JQ.ajax
    (apiRoot <> (pack $ A.companies ++ "/" ++ A.map'))
    (callback . (map (\(a,b,c,d) -> (a,b,toMaybe c,toMaybe d))) . items)
    noopOnError

createCompany :: C.Company
              -> Maybe C.Coordinates
              -> (C.CompanyId -> Fay ())

              -> Fay ()
createCompany company coordinates callback = ajax
  (company, toMyMaybe coordinates)
  (pack A.companies)
  post
  callback

createMachine :: M.Machine 
              -> C.CompanyId
              -> MT.MyEither
              -> Maybe CP.ContactPersonId
              -> Maybe M.MachineId
              -> [(EF.ExtraFieldId, Text)]
              -> Fay ()
              -> Fay ()
createMachine machine companyId machineType contactPersonId linkedMachineId extraFields callback = ajax
  (machine, machineType, toMyMaybe contactPersonId, toMyMaybe linkedMachineId, extraFields)
  (pack $ A.companies ++ "/" ++ A.single ++ "/" ++ (show $ C.getCompanyId companyId) ++ "/" ++ A.machines)
  post
  (const callback)

updateEmployee :: E.EmployeeId
               -> E.Employee
               -> Fay ()
               -> Fay ()
updateEmployee employeeId employee callback = ajax
  employee
  (pack $ A.employees ++ "/" ++ (show $ E.getEmployeeId employeeId))
  put
  (const callback)

updateContactPerson :: CP.ContactPersonId
                    -> CP.ContactPerson
                    -> Fay ()
                    -> Fay ()
updateContactPerson cpId cp callback = ajax
  cp
  (pack $ A.contactPersons ++ "/" ++ (show $ CP.getContactPersonId cpId))
  put
  (const callback)

updateCompany :: C.CompanyId
              -> C.Company
              -> Maybe C.Coordinates
              -> Fay ()
              -> Fay ()
updateCompany companyId company coordinates callback = ajax
  (company, toMyMaybe coordinates)
  (pack $ A.companies ++ "/" ++ A.single ++ "/" ++ (show $ C.getCompanyId companyId))
  put
  (const $ callback)

updateUpkeep :: (U.Upkeep', Maybe E.EmployeeId)
             -> Fay ()
             -> Fay ()
updateUpkeep ((upkeepId, upkeep, upkeepMachines),maybeEmployeeId) callback = 
  ajax
    (upkeep, upkeepMachines, toMyMaybe maybeEmployeeId)
    (pack $ A.upkeep ++ "/" ++ A.single ++ "/" ++ (show $ U.getUpkeepId upkeepId))
    put
    (const callback)

updateMachineType :: (MT.MachineTypeId, MT.MachineType, [US.UpkeepSequence])
                  -> Fay ()
                  -> Fay ()
updateMachineType (machineTypeId, machineType, upkeepSequences) callback = ajax
  (machineType, upkeepSequences)
  (pack $ A.machineTypes ++ "/" ++ A.byId ++ "/" ++ (show $ MT.getMachineTypeId machineTypeId))
  put
  (const callback)

updateMachine :: M.MachineId -- ^ machine id
              -> M.Machine
              -> Maybe M.MachineId -- ^ linked machine id
              -> [(EF.ExtraFieldId, Text)]
              -> Fay ()
              -> Fay ()
updateMachine machineId machine linkedMachineId machineSpecificData callback = ajax
  (machine, toMyMaybe linkedMachineId, machineSpecificData)
  (pack $ A.machines ++ "/" ++ (show $ M.getMachineId machineId))
  put
  (const callback)

saveExtraFieldSettings :: [(MK.MachineKindEnum, [(EF.ExtraFieldIdentification, MK.MachineKindSpecific)])]
                       -> Fay ()
                       -> Fay ()
saveExtraFieldSettings data' callback = ajax
  data'
  (pack $ A.machineKind ++ "/()")
  put
  (const callback)

createUpkeep :: (U.Upkeep, [UM.UpkeepMachine'], Maybe E.EmployeeId)
             -> C.CompanyId -- ^ company id
             -> Fay ()
             -> Fay ()
createUpkeep (newUpkeep,upkeepMachines,maybeEmployeeId) companyId callback =
  ajax 
    (newUpkeep, upkeepMachines, toMyMaybe maybeEmployeeId)
    (pack $ A.companies ++ "/" ++ A.single ++ "/" ++ (show $ C.getCompanyId companyId) ++ "/" ++ A.upkeep)
    post
    (const callback)

createEmployee :: E.Employee
               -> Fay ()
               -> Fay ()
createEmployee employee callback =
  ajax
    employee
    (pack $ A.employees)
    post
    (const callback)

createContactPerson :: C.CompanyId
                    -> CP.ContactPerson
                    -> Fay ()
                    -> Fay ()
createContactPerson companyId contactPerson callback =
  ajax
    contactPerson
    (pack $ A.companies ++ "/" ++ A.single ++ "/" ++ (show $ C.getCompanyId companyId) ++ "/" ++ A.contactPersons)
    post
    (const callback)

uploadPhotoData :: File
                -> M.MachineId
                -> (P.PhotoId -> Fay ())
                -> Fay ()
uploadPhotoData fileContents machineId callback =
  JQ.ajax' $ JQ.defaultAjaxSettings {
    JQ.success = Defined callback ,
    JQ.data' = Defined fileContents ,
    JQ.url = Defined $ apiRoot <> pack (A.machines ++ "/" ++ (show $ M.getMachineId machineId) ++
      "/" ++ A.photos) ,
    JQ.type' = Defined post ,
    JQ.processData = Defined False ,
    JQ.contentType = Defined $ pack "application/x-www-form-urlencoded" }

uploadPhotoMeta :: PM.PhotoMeta
                -> P.PhotoId
                -> Fay ()
                -> Fay ()
uploadPhotoMeta photoMeta photoId callback = ajax
  photoMeta
  (pack $ A.photoMeta ++ "/" ++ (show $ P.getPhotoId photoId))
  put
  (const callback)
