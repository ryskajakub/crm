{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Server (
  fetchCompanies
  , fetchMachines
) where

import FFI (ffi, Automatic)
import Crm.Shared.Company (Company)
import Crm.Shared.Machine (Machine)
import qualified Crm.Shared.Api as A
import "fay-base" Prelude
import "fay-base" Data.Text (Text, pack)

data CrmApi

fetchCompanies :: ([(Int, Company)] -> Fay ())
               -> Fay ()
fetchCompanies var = fetch var (pack A.companiesClient)

fetchMachines :: ([(Int, Machine)] -> Fay ())
              -> Fay ()
fetchMachines var = fetch var (pack A.machinesClient)

fetch :: ([a] -> Fay ())
      -> Text
      -> Fay ()
fetch setData restApiNode = do
  crmApi <- crmApiFacade
  fetch' crmApi restApiNode setData

fetch' :: CrmApi -- ^ Pointer to Crm api phantom
       -> Text -- ^ Model to fetch
       -> ([Automatic a] -> Fay ()) -- ^ Callback ran on the fetched data
       -> Fay ()
fetch' = ffi "\
\ %1[%2]['list'](function(d) {\
  \ %3(d.items); \
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
