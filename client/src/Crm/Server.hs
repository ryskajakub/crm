{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Server (
  fetchMachine , 
  fetchUpkeeps , 
  createCompany , 
  createMachine , 
  createUpkeep , 
  updateMachine , 
  fetchPlannedUpkeeps , 
  fetchFrontPageData , 
  fetchMachineTypesAutocomplete ,
  fetchMachineType ,
  fetchUpkeep ,
  fetchCompany ) where

import FFI (ffi, Automatic, Defined(Defined))
import "fay-base" Prelude hiding (putStrLn)
import "fay-base" Data.Text (Text, pack, showInt, (<>), putStrLn)
import "fay-base" Data.Maybe (listToMaybe)

import qualified JQuery as JQ

import qualified Crm.Shared.Company as C
import qualified Crm.Shared.Upkeep as U
import qualified Crm.Shared.Machine as M
import qualified Crm.Shared.MachineType as MT
import qualified Crm.Shared.Api as A
import qualified Crm.Shared.YearMonthDay as YMD

data CrmApi

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
                 -> (Maybe (Int, MT.MachineType) -> Fay ()) -- ^ callback
                 -> Fay ()
fetchMachineType machineTypeName callback = 
  JQ.ajax
    (pack "/api/v1.0.0/" <> pack A.machineTypes <> pack "/by-type/" <> machineTypeName <> pack "/")
    (\maybeMachineType -> case maybeMachineType of
      [] -> callback Nothing
      x:_ -> callback $ Just x)
    (const $ const $ const $ return ())

fetchUpkeep :: Int -- ^ upkeep id
            -> ((U.Upkeep, [(Int, M.Machine, Int, MT.MachineType)]) -> Fay ())
            -> Fay ()
fetchUpkeep upkeepId callback =
  JQ.ajax
    (pack "/api/v1.0.0/" <> pack A.upkeep <> pack "/single/" <> showInt upkeepId <> pack "/")
    callback
    noopOnError

fetchUpkeeps :: Int -- ^ company id
             -> ([(Int, U.Upkeep)] -> Fay ()) -- ^ callback
             -> Fay ()
fetchUpkeeps companyId callback = 
  JQ.ajax
    (pack "/api/v1.0.0/companies/" <> showInt companyId <> pack "/upkeeps/")
    (callback . items)
    (const $ const $ const $ return ())

fetchMachine :: Int -- ^ machine id
             -> ((M.Machine, Int, MT.MachineType, YMD.YearMonthDay) -> Fay()) -- ^ callback
             -> Fay ()
fetchMachine machineId callback = 
  JQ.ajax
    (pack "/api/v1.0.0/machines/" <> showInt machineId <> pack "/")
    callback
    (const $ const $ const $ return ())

fetchCompany :: Int -- ^ company id
             -> ((C.Company, [(Int, M.Machine, Int, MT.MachineType)]) -> Fay ()) -- ^ callback
             -> Fay ()
fetchCompany companyId callback =
  JQ.ajax
    (pack "/api/v1.0.0/" <> pack A.companies <> pack "/" <> showInt companyId <> pack "/")
    callback
    (const $ const $ const $ return ())

data Items

-- | Unpack the outermost layer of the fetched list in order to get to the data
items :: Items -> Automatic a
items = ffi " %1['items'] "

fetchFrontPageData :: ([(Int, C.Company, Maybe YMD.YearMonthDay)] -> Fay ())
                   -> Fay ()
fetchFrontPageData callback = let
  lMb [] = []
  lMb ((a,b,x) : xs) = (a,b,listToMaybe x) : lMb xs
  in JQ.ajax
    (pack "/api/v1.0.0/" <> pack A.companies <> pack "/")
    (callback . lMb . items)
    (const $ const $ const $ return ())

fetchPlannedUpkeeps :: ([(Int, U.Upkeep, Int, C.Company)] -> Fay ())
                    -> Fay ()
fetchPlannedUpkeeps callback =
  JQ.ajax
    (pack "/api/v1.0.0/" <> pack A.upkeep <> pack "/" <> pack A.planned <> pack "/")
    (callback . items)
    (const $ const $ const $ return ())

createCompany :: C.Company
              -> (Int -> Fay())
              -> Fay ()
createCompany company callback = do
  crmApi <- crmApiFacade
  create' crmApi (pack A.companiesClient) company callback

createMachine :: M.Machine 
              -> MT.MachineType
              -> (Int -> Fay())
              -> Fay ()
createMachine machine machineType callback =
  ajax
    (machine, machineType)
    (pack "/api/v1.0.0/companies/" <> (showInt $ M.companyId machine) <> pack "/machines/")
    (pack "POST")
    callback

ajax :: a -- data to send
     -> Text -- url
     -> Text -- method -- GET | POST
     -> (b -> Fay ()) -- callback
     -> Fay ()
ajax data' url method callback = JQ.ajax' $ JQ.defaultAjaxSettings {
  JQ.success = Defined $ callback ,
  JQ.data' = Defined data' ,
  JQ.url = Defined $ url ,
  JQ.type' = Defined $ method ,
  JQ.processData = Defined False ,
  JQ.contentType = Defined $ pack "application/json" ,
  JQ.dataType = Defined $ pack "json" }

updateMachine :: Int -- machine id
              -> M.Machine
              -> Fay ()
              -> Fay ()
updateMachine machineId machine callback = ajax
  machine
  (pack "/api/v1.0.0/" <> pack A.machines <> pack "/" <> showInt machineId <> pack "/")
  (pack "PUT")
  (const callback)

createUpkeep :: U.Upkeep
             -> Int -- ^ company id
             -> (Int -> Fay ())
             -> Fay ()
createUpkeep upkeep companyId callback =
  ajax 
    upkeep
    (pack "/api/v1.0.0/" <> pack A.companies <> pack "/" <>
      showInt companyId <> pack "/" <> pack A.upkeep <> pack "/")
    (pack "POST")
    callback

fetch :: ([a] -> Fay ())
      -> Text
      -> Fay ()
fetch setData restApiNode = do
  crmApi <- crmApiFacade
  fetch' crmApi restApiNode setData

fetch' :: CrmApi -- ^ pointer to Crm api phantom
       -> Text -- ^ type of model to fetch
       -> ([Automatic a] -> Fay ()) -- ^ Callback ran on the fetched data
       -> Fay ()
fetch' = ffi "\
\ %1[%2]['list'](function(d) {\
  \ %3(d.items); \
\ })\
\ "

create' :: CrmApi -- ^ pointer to crm api phantom
        -> Text -- ^ type of model to create
        -> Automatic a -- ^ model to create on the server
        -> (Int -> Fay()) -- ^ callback taking id of the newly created data
        -> Fay ()
create' = ffi "\
\ %1[%2]['create'](%3, function(id) {\
  \ %4(id);\
\ })\
\ "

crmApiFacade :: Fay CrmApi
crmApiFacade = ffi "\
\ (function() {\
  \ var CrmApi = require('./CrmApi');\
  \ var crmApi = new CrmApi('/api');\
  \ return crmApi;\
\ })() \
\ "
