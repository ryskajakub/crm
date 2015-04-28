{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Server (
  createCompany , 
  createMachine , 
  createUpkeep , 
  createEmployee ,
  updateUpkeep ,
  updateMachine , 
  updateCompany ,
  updateMachineType , 
  uploadPhotoData ,
  uploadPhotoMeta ,
  fetchMachine , 
  fetchMachinePhotos ,
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
  deleteUpkeep ,
  deleteCompany ,
  deleteMachine ,
  getPhoto ) where

import FFI (ffi, Automatic, Defined(Defined, Undefined))
import "fay-base" Prelude hiding (putStrLn)
import "fay-base" Data.Text (Text, (<>), unpack, pack)

import qualified JQuery as JQ

import qualified Crm.Shared.Company as C
import qualified Crm.Shared.Upkeep as U
import qualified Crm.Shared.Machine as M
import qualified Crm.Shared.MachineType as MT
import qualified Crm.Shared.UpkeepMachine as UM
import qualified Crm.Shared.Api as A
import qualified Crm.Shared.Photo as P
import qualified Crm.Shared.PhotoMeta as PM
import qualified Crm.Shared.YearMonthDay as YMD
import qualified Crm.Shared.Employee as E
import qualified Crm.Shared.UpkeepSequence as US
import qualified Crm.Shared.Direction as DIR
import Crm.Shared.MyMaybe

import Crm.Helpers (File)

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
  (pack $ A.companies ++ "/" ++ (show $ C.getCompanyId companyId))
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
    (apiRoot <> (pack $ A.companies ++ "/" ++ (show $ C.getCompanyId companyId) ++ "/" ++ A.upkeep))
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

fetchMachine :: M.MachineId -- ^ machine id
             -> ((C.CompanyId, M.Machine, MT.MachineTypeId,
                (MT.MachineType, [US.UpkeepSequence]), YMD.YearMonthDay, 
                [(U.UpkeepId, U.Upkeep, UM.UpkeepMachine, Maybe E.Employee)]) -> Fay()) -- ^ callback
             -> Fay ()
fetchMachine machineId callback = let
  fun2 (a,b,c,d) = (a,b,c,toMaybe d)
  fun (a,b,c,d,e,g) = (a,b,c,d,e,map fun2 g)
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

fetchCompany :: C.CompanyId -- ^ company id
             -> ((C.Company, [(M.MachineId, M.Machine, C.CompanyId, 
               MT.MachineTypeId, MT.MachineType)]) -> Fay ()) -- ^ callback
             -> Fay ()
fetchCompany companyId callback =
  JQ.ajax
    (apiRoot <> (pack $ A.companies ++ "/" ++ (show $ C.getCompanyId companyId)))
    callback
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

createCompany :: C.Company
              -> Fay ()
              -> Fay ()
createCompany company callback = ajax
  company
  (pack A.companies)
  post
  (const callback)

createMachine :: M.Machine 
              -> C.CompanyId
              -> MT.MyEither
              -> Fay ()
              -> Fay ()
createMachine machine companyId machineType callback =
  ajax
    (machine, machineType)
    (pack $ A.companies ++ "/" ++ (show $ C.getCompanyId companyId) ++ "/" ++ A.machines)
    post
    (const callback)

updateCompany :: C.CompanyId
              -> C.Company
              -> Fay ()
updateCompany companyId company = ajax
  (company)
  (pack $ A.companies ++ "/" ++ (show $ C.getCompanyId companyId))
  put
  (const $ return ())

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
              -> Fay ()
              -> Fay ()
updateMachine machineId machine callback = ajax
  machine
  (pack $ A.machines ++ "/" ++ (show $ M.getMachineId machineId))
  put
  (const callback)

createUpkeep :: (U.Upkeep, [UM.UpkeepMachine'], Maybe E.EmployeeId)
             -> C.CompanyId -- ^ company id
             -> Fay ()
             -> Fay ()
createUpkeep (newUpkeep,upkeepMachines,maybeEmployeeId) companyId callback =
  ajax 
    (newUpkeep, upkeepMachines, toMyMaybe maybeEmployeeId)
    (pack $ A.companies ++ "/" ++ (show $ C.getCompanyId companyId) ++ "/" ++ A.upkeep)
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
