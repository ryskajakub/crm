{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Server (
  createCompany , 
  createMachine , 
  createUpkeep , 
  updateUpkeep ,
  updateMachine , 
  fetchMachine , 
  fetchUpkeeps , 
  fetchPlannedUpkeeps , 
  fetchFrontPageData , 
  fetchMachineTypesAutocomplete ,
  fetchMachineType ,
  fetchUpkeep ,
  fetchCompany ) where

import FFI (ffi, Automatic, Defined(Defined))
import "fay-base" Prelude hiding (putStrLn)
import "fay-base" Data.Text (Text, (<>), unpack, pack)
import "fay-base" Data.Maybe (listToMaybe)

import qualified JQuery as JQ

import qualified Crm.Shared.Company as C
import qualified Crm.Shared.Upkeep as U
import qualified Crm.Shared.Machine as M
import qualified Crm.Shared.MachineType as MT
import qualified Crm.Shared.UpkeepMachine as UM
import qualified Crm.Shared.Api as A
import qualified Crm.Shared.YearMonthDay as YMD

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

ajax :: a -- data to send
     -> Text -- url
     -> Text -- method PUT | POST
     -> (b -> Fay ()) -- callback
     -> Fay ()
ajax data' url method callback = JQ.ajax' $ JQ.defaultAjaxSettings {
  JQ.success = Defined callback ,
  JQ.data' = Defined data' ,
  JQ.url = Defined $ apiRoot <> url ,
  JQ.type' = Defined method ,
  JQ.processData = Defined False ,
  JQ.contentType = Defined $ pack "application/json" ,
  JQ.dataType = Defined $ pack "json" }


fetchMachineTypesAutocomplete :: Text -- ^ the string user typed
                              -> ([Text] -> Fay ()) -- callback filled with option that the user can pick
                              -> Fay ()
fetchMachineTypesAutocomplete text callback = do
  JQ.ajax
    (apiRoot <> (pack $ A.machineTypes ++ "/" ++ A.autocomplete ++ "/" ++ unpack text))
    (callback . items)
    noopOnError

fetchMachineType :: Text -- ^ machine type exact match
                 -> (Maybe (MT.MachineTypeId, MT.MachineType) -> Fay ()) -- ^ callback
                 -> Fay ()
fetchMachineType machineTypeName callback = 
  JQ.ajax
    (apiRoot <> (pack $ A.machineTypes ++ "/" ++ A.byType ++ "/" ++ unpack machineTypeName))
    (\maybeMachineType -> case maybeMachineType of
      [] -> callback Nothing
      x:_ -> callback $ Just x)
    noopOnError

fetchUpkeep :: U.UpkeepId -- ^ upkeep id
            -> ((C.CompanyId, (U.Upkeep, [UM.UpkeepMachine']), [(M.MachineId, M.Machine, 
                 C.CompanyId, MT.MachineTypeId, MT.MachineType)]) -> Fay ()) 
            -> Fay ()
fetchUpkeep upkeepId callback =
  JQ.ajax
    (apiRoot <> (pack $ A.upkeep ++ "/" ++ A.single ++ "/" ++ (show $ U.getUpkeepId upkeepId) ++ "/"))
    callback
    noopOnError

fetchUpkeeps :: C.CompanyId -- ^ company id
             -> ([(U.Upkeep,[UM.UpkeepMachine'])] -> Fay ()) -- ^ callback
             -> Fay ()
fetchUpkeeps companyId callback = 
  JQ.ajax
    (apiRoot <> (pack $ A.companies ++ "/" ++ (show $ C.getCompanyId companyId) ++ "/" ++ A.upkeep))
    (callback . items)
    noopOnError

fetchMachine :: M.MachineId -- ^ machine id
             -> ((M.Machine, MT.MachineTypeId, M.MachineId, MT.MachineType, YMD.YearMonthDay) -> Fay()) -- ^ callback
             -> Fay ()
fetchMachine machineId callback = 
  JQ.ajax
    (pack $ A.machines ++ "/" ++ (show $ M.getMachineId machineId))
    callback
    noopOnError

fetchCompany :: C.CompanyId -- ^ company id
             -> ((C.Company, [(M.MachineId, M.Machine, C.CompanyId, MT.MachineTypeId, MT.MachineType)]) -> Fay ()) -- ^ callback
             -> Fay ()
fetchCompany companyId callback =
  JQ.ajax
    (apiRoot <> (pack $ A.companies ++ "/" ++ (show $ C.getCompanyId companyId)))
    callback
    noopOnError

fetchFrontPageData :: ([(C.CompanyId, C.Company, Maybe YMD.YearMonthDay)] -> Fay ())
                   -> Fay ()
fetchFrontPageData callback = let
  lMb [] = []
  lMb ((a,b,x) : xs) = (a,b,listToMaybe x) : lMb xs
  in JQ.ajax
    (apiRoot <> pack A.companies)
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
    (pack $ (show $ C.getCompanyId companyId) ++ "/" ++ A.machines)
    post
    (const callback)

updateUpkeep :: U.Upkeep'
             -> Fay ()
             -> Fay ()
updateUpkeep (upkeepId, upkeep, upkeepMachines) callback = ajax
  (upkeep, upkeepMachines)
  (pack $ A.companies ++ "/0/" ++ A.upkeep ++ "/" ++ (show $ U.getUpkeepId upkeepId))
  put
  (const callback)

updateMachine :: M.MachineId -- ^ machine id
              -> MT.MachineTypeId -- ^ machine type id
              -> M.Machine
              -> Fay ()
              -> Fay ()
updateMachine machineId machineTypeId machine callback = ajax
  (machineTypeId, machine)
  (pack $ A.machines ++ "/" ++ (show $ M.getMachineId machineId))
  put
  (const callback)

createUpkeep :: (U.Upkeep, [UM.UpkeepMachine'])
             -> C.CompanyId -- ^ company id
             -> Fay ()
             -> Fay ()
createUpkeep newUpkeep companyId callback =
  ajax 
    newUpkeep
    (pack $ A.companies ++ "/" ++ (show $ C.getCompanyId companyId) ++ "/" ++ A.upkeep)
    post
    (const callback)
