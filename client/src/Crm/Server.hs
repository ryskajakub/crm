module Crm.Server where

import FFI (ffi)

fetchFromServer :: Fay ()
fetchFromServer = do
  crmApi <- crmApiFacade
  printlnApi crmApi

data CrmApi

printlnApi :: CrmApi -> Fay ()
printlnApi = ffi " %1['Company']['list'](function(data) {console.log(data);}) "

crmApiFacade :: Fay CrmApi
crmApiFacade = ffi "\
\ (function() {\
  \ var CrmApi = require('./CrmApi');\
  \ var crmApi = new CrmApi('/api');\
  \ return crmApi;\ 
\ })() \
\ "
