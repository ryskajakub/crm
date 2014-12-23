{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Server (
  fetchCompanies
  , fetchMachines, fetchMachine
  , fetchUpkeeps
  , createCompany
  , createMachine
  , createUpkeep
  , updateMachine
  , fetchPlannedUpkeeps
  , fetchFrontPageData
) where

import FFI (ffi, Automatic, Defined(Defined), Nullable)
import Crm.Shared.Company (Company)
import qualified Crm.Shared.Upkeep as U
import qualified Crm.Shared.Machine as M
import qualified Crm.Shared.Api as A
import qualified Crm.Shared.YearMonthDay as YMD
import "fay-base" Prelude
import "fay-base" Data.Text (Text, pack, append, showInt, unpack, (<>))
import "fay-base" Data.Maybe (listToMaybe)
import qualified JQuery as JQ

import Debug.Trace

data CrmApi

fetchCompanies :: ([(Int, Company)] -> Fay ())
               -> Fay ()
fetchCompanies var = fetch var (pack A.companiesClient)

fetchMachines :: ([(Int, M.Machine)] -> Fay ())
              -> Fay ()
fetchMachines var = fetch var (pack A.machinesClient)

fetchUpkeeps :: ([(Int, U.Upkeep)] -> Fay ())
             -> Fay ()
fetchUpkeeps var = fetch var (pack A.upkeepsClient)

fetchMachine :: Int -- ^ machine id
             -> ((M.Machine, YMD.YearMonthDay) -> Fay()) -- ^ callback
             -> Fay ()
fetchMachine machineId callback = 
  JQ.ajax
    (pack "/api/v1.0.0/machines/" <> showInt machineId <> pack "/")
    callback
    (const $ const $ const $ return ())

data Items

-- | Unpack the outermost layer of the fetched list in order to get to the data
items :: Items -> Automatic a
items = ffi " %1['items'] "

fetchFrontPageData :: ([(Int, Company, Maybe YMD.YearMonthDay)] -> Fay ())
                   -> Fay ()
fetchFrontPageData callback = let
  lMb [] = []
  lMb ((a,b,x) : xs) = (a,b,listToMaybe x) : lMb xs
  in JQ.ajax
    (pack "/api/v1.0.0/" <> pack A.companies <> pack "/")
    (callback . lMb . items)
    (const $ const $ const $ return ())

fetchPlannedUpkeeps :: ([(Int, U.Upkeep, Int, Company)] -> Fay ())
                    -> Fay ()
fetchPlannedUpkeeps callback =
  JQ.ajax
    (pack "/api/v1.0.0/" <> pack A.upkeep <> pack "/" <> pack A.planned <> pack "/")
    (callback . items)
    (const $ const $ const $ return ())

createCompany :: Company
              -> (Int -> Fay())
              -> Fay ()
createCompany company callback = do
  crmApi <- crmApiFacade
  create' crmApi (pack A.companiesClient) company callback

createMachine :: M.Machine
              -> (Int -> Fay())
              -> Fay ()
createMachine machine callback =
  ajax
    machine
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

updateMachine :: (Int, M.Machine)
              -> Fay ()
              -> Fay ()
updateMachine (machineId, machine) callback = ajax
  machine
  (pack "/api/v1.0.0/" <> pack A.machines <> pack "/" <> showInt machineId <> pack "/")
  (pack "PUT")
  (const $ return ())

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
