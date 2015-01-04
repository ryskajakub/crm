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
import "fay-base" Data.Text (Text, pack, showInt, (<>))
import "fay-base" Data.Maybe (listToMaybe)

import qualified JQuery as JQ

import qualified Crm.Shared.Company as C
import qualified Crm.Shared.Upkeep as U
import qualified Crm.Shared.Machine as M
import qualified Crm.Shared.MachineType as MT
import qualified Crm.Shared.UpkeepMachine as UM
import qualified Crm.Shared.Api as A
import qualified Crm.Shared.YearMonthDay as YMD

data CrmApi

noopOnError :: a -> b -> c -> Fay ()
noopOnError = (const $ const $ const $ return ())

fetchMachineTypesAutocomplete :: Text -- ^ the string user typed
                              -> ([Text] -> Fay ()) -- callback filled with option that the user can pick
                              -> Fay ()
fetchMachineTypesAutocomplete text callback = do
  JQ.ajax
    (pack "/api/v1.0.0/" <> pack A.machineTypes <> pack "/autocomplete/" <> text)
    (callback . items)
    (const $ const $ const $ return ())

fetchMachineType :: Text -- ^ machine type exact match
                 -> (Maybe (MT.MachineTypeId, MT.MachineType) -> Fay ()) -- ^ callback
                 -> Fay ()
fetchMachineType machineTypeName callback = 
  JQ.ajax
    (pack "/api/v1.0.0/" <> pack A.machineTypes <> pack "/by-type/" <> machineTypeName <> pack "/")
    (\maybeMachineType -> case maybeMachineType of
      [] -> callback Nothing
      x:_ -> callback $ Just x)
    (const $ const $ const $ return ())

fetchUpkeep :: U.UpkeepId -- ^ upkeep id
            -> ((C.CompanyId, (U.Upkeep, [UM.UpkeepMachine']), [(M.MachineId, M.Machine, 
                 C.CompanyId, MT.MachineTypeId, MT.MachineType)]) -> Fay ()) 
            -> Fay ()
fetchUpkeep upkeepId callback =
  JQ.ajax
    (pack "/api/v1.0.0/" <> pack A.upkeep <> pack "/single/" <> (showInt $ U.getUpkeepId upkeepId) <> pack "/")
    callback
    noopOnError

fetchUpkeeps :: C.CompanyId -- ^ company id
             -> ([(U.Upkeep,[UM.UpkeepMachine'])] -> Fay ()) -- ^ callback
             -> Fay ()
fetchUpkeeps companyId callback = 
  JQ.ajax
    (pack "/api/v1.0.0/companies/" <> (showInt $ C.getCompanyId companyId) <> pack "/upkeeps/")
    (callback . items)
    (const $ const $ const $ return ())

fetchMachine :: M.MachineId -- ^ machine id
             -> ((M.Machine, MT.MachineTypeId, M.MachineId, MT.MachineType, YMD.YearMonthDay) -> Fay()) -- ^ callback
             -> Fay ()
fetchMachine machineId callback = 
  JQ.ajax
    (pack "/api/v1.0.0/machines/" <> (showInt $ M.getMachineId machineId) <> pack "/")
    callback
    (const $ const $ const $ return ())

fetchCompany :: C.CompanyId -- ^ company id
             -> ((C.Company, [(M.MachineId, M.Machine, C.CompanyId, MT.MachineTypeId, MT.MachineType)]) -> Fay ()) -- ^ callback
             -> Fay ()
fetchCompany companyId callback =
  JQ.ajax
    (pack "/api/v1.0.0/" <> pack A.companies <> pack "/" <> (showInt $ C.getCompanyId companyId) <> pack "/")
    callback
    (const $ const $ const $ return ())

data Items

-- | Unpack the outermost layer of the fetched list in order to get to the data
items :: Items -> Automatic a
items = ffi " %1['items'] "

fetchFrontPageData :: ([(C.CompanyId, C.Company, Maybe YMD.YearMonthDay)] -> Fay ())
                   -> Fay ()
fetchFrontPageData callback = let
  lMb [] = []
  lMb ((a,b,x) : xs) = (a,b,listToMaybe x) : lMb xs
  in JQ.ajax
    (pack "/api/v1.0.0/" <> pack A.companies <> pack "/")
    (callback . lMb . items)
    (const $ const $ const $ return ())

fetchPlannedUpkeeps :: ([(U.UpkeepId, U.Upkeep, C.CompanyId, C.Company)] -> Fay ())
                    -> Fay ()
fetchPlannedUpkeeps callback =
  JQ.ajax
    (pack "/api/v1.0.0/" <> pack A.upkeep <> pack "/" <> pack A.planned <> pack "/")
    (callback . items)
    (const $ const $ const $ return ())

createCompany :: C.Company
              -> Fay ()
              -> Fay ()
createCompany company callback = ajax
  company
  (apiRoot <> pack A.companies <> pack "/")
  (pack "POST")
  (const callback)

createMachine :: M.Machine 
              -> C.CompanyId
              -> MT.MyEither
              -> (Int -> Fay())
              -> Fay ()
createMachine machine companyId machineType callback =
  ajax
    (machine, machineType)
    (pack "/api/v1.0.0/companies/" <> (showInt $ C.getCompanyId companyId) <> pack "/machines/")
    (pack "POST")
    callback

ajax :: a -- data to send
     -> Text -- url
     -> Text -- method
     -> (b -> Fay ()) -- callback
     -> Fay ()
ajax data' url method callback = JQ.ajax' $ JQ.defaultAjaxSettings {
  JQ.success = Defined callback ,
  JQ.data' = Defined data' ,
  JQ.url = Defined url ,
  JQ.type' = Defined method ,
  JQ.processData = Defined False ,
  JQ.contentType = Defined $ pack "application/json" ,
  JQ.dataType = Defined $ pack "json" }

apiRoot :: Text
apiRoot = pack "/api/v1.0.0/"

updateUpkeep :: U.Upkeep'
             -> Fay ()
             -> Fay ()
updateUpkeep (upkeepId, upkeep, upkeepMachines) callback = ajax
  (upkeep, upkeepMachines)
  (apiRoot <> pack "companies/0/" <> pack A.upkeep <> pack "/" 
    <> (showInt $ U.getUpkeepId upkeepId) <> pack "/")
  (pack "PUT")
  (const callback)

updateMachine :: M.MachineId -- ^ machine id
              -> MT.MachineTypeId -- ^ machine type id
              -> M.Machine
              -> Fay ()
              -> Fay ()
updateMachine machineId machineTypeId machine callback = ajax
  (machineTypeId, machine)
  (apiRoot <> pack A.machines <> pack "/" <> (showInt $ M.getMachineId machineId) <> pack "/")
  (pack "PUT")
  (const callback)

createUpkeep :: (U.Upkeep, [UM.UpkeepMachine'])
             -> C.CompanyId -- ^ company id
             -> (U.UpkeepId -> Fay ())
             -> Fay ()
createUpkeep newUpkeep companyId callback =
  ajax 
    newUpkeep
    (pack "/api/v1.0.0/" <> pack A.companies <> pack "/" <>
      (showInt $ C.getCompanyId companyId) <> pack "/" <> pack A.upkeep <> pack "/")
    (pack "POST")
    callback

