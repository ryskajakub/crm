{-# LANGUAGE PackageImports #-}

module HaskellReact.BackboneRouter where

import "fay-base" Data.Text (Text)
import FFI (ffi)

-- | Starts the Backbone router
startRouter :: [(Text, [Text] -> Fay ())] -- ^ list of routes and callback handlers, the argument to the callback is the params of the route
            -> Fay ()
startRouter = ffi " \
\ (function () {\
  \ var Backbone = require('backbone');\
  \ var routesFromFay = %1;\
  \ var routes = {};\
  \ for (var i = 0; i < routesFromFay.length; i++){\
    \ routes[routesFromFay[i][0]] = (function(i) { return function() { routesFromFay[i][1](arguments); }; })(i)\
  \ }\
  \ var router = new Backbone.Router({\
    \ routes: routes\
  \ });\
  \ Backbone.history.start();\
\ })()\
\ "
