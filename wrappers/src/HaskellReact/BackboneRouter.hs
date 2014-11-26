module HaskellReact.BackboneRouter where

import FFI (ffi)

startRouter :: Fay ()
startRouter = ffi " \
\ (function () {\
  \ console.log('component mounted');\
  \ var Backbone = require('backbone');\
  \ var router = new Backbone.Router({\
    \ routes: {\
      \ 'help/:param/:param2': function(param2, paramxxx) {\
        \ console.log(param2, paramxxx);\
      \ }\
    \ } \
  \ });\
  \ Backbone.history.start();\
\ })()\
\ "
