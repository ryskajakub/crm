{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Server where

import FFI (ffi, Automatic)
import Crm.Shared.Company
import Data.Var
import "fay-base" Prelude
import "fay-base" Data.Text (Text, pack)

fetchFromServer :: Var (Maybe [Company]) -> Fay ()
fetchFromServer companiesVar = do
  crmApi <- crmApiFacade
  fetchCompanies crmApi (\companies -> do
    set companiesVar $ Just companies)

data CrmApi

fetchCompanies :: CrmApi
               -> ([Company] -> Fay ())
               -> Fay ()
fetchCompanies crmApi callback = fetch crmApi (pack "Companies") callback

fetch :: CrmApi -- ^ Pointer to Crm api phantom
      -> Text -- ^ Model to fetch
      -> ([Automatic a] -> Fay ()) -- ^ Callback ran on the fetched data
      -> Fay ()
fetch = ffi "\
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
