{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Server (
  fetchCompanies
  , fetchMachines, fetchMachine
  , fetchUpkeeps
  , createCompany
  , createMachine
  , createUpkeep
) where

import FFI (ffi, Automatic)
import Crm.Shared.Company (Company)
import qualified Crm.Shared.Upkeep as U
import qualified Crm.Shared.Machine as M
import qualified Crm.Shared.Api as A
import qualified Crm.Shared.YearMonthDay as D
import "fay-base" Prelude
import "fay-base" Data.Text (Text, pack, append, showInt, unpack, (<>))
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
             -> (M.Machine -> Fay()) -- ^ callback
             -> Fay ()
fetchMachine machineId callback = 
  JQ.ajax
    (pack "/api/v1.0.0/machines/" <> showInt machineId <> pack "/")
    callback
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
  JQ.ajaxPost
    (pack "/api/v1.0.0/companies/" `append` (showInt $ M.companyId machine) `append` pack "/machines/")
    machine
    callback
    (const $ const $ const $ return ())

createUpkeep :: U.Upkeep
             -> Int -- ^ company id
             -> (Int -> Fay ())
             -> Fay ()
createUpkeep upkeep companyId callback =
  JQ.ajaxPost 
    (pack "/api/v1.0.0/" `append`
      pack A.companies `append`
      pack "/" `append`
      showInt companyId `append`
      pack "/" `append` 
      pack A.upkeep `append`
      pack "/")
    upkeep
    callback
    (const $ const $ const $ return ())

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
