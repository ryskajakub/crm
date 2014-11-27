{-# LANGUAGE PackageImports #-}

module Crm.Server where

import FFI (ffi)
import Crm.Component.Data
import "fay-base" Data.Text (unpack, pack)
import Data.Var

fetchFromServer :: (Var [Company]) -> Fay ()
fetchFromServer companiesVar = do
  crmApi <- crmApiFacade
  fetchCompanies crmApi (\companies -> do
    set companiesVar companies)

data CrmApi

fetchCompanies :: CrmApi -- ^ Pointer to Crm api phantom
               -> ([Company] -> Fay ()) -- ^ Callback ran on the fetched data
               -> Fay ()
fetchCompanies = ffi "\
\ %1['Company']['list'](function(d) {\
  \ var data = d.items;\
  \ var items = [];\
  \ for (var i = 0; i < data.length - 1; i++ ){\
    \ items[i] = data[i][1];\
    \ items[i]['id'] = data[i][0];\
    \ items[i]['instance'] = 'company';\
  \ }\
  \ %2(items); \
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
