{-# LANGUAGE PackageImports #-}

module HaskellReact.BackboneRouter where

import "fay-base" Data.Text (Text, pack)
import FFI (ffi, Defined(Defined, Undefined))
import HaskellReact
import HaskellReact.Tag.Hyperlink (mkAAttrs, href, a'')

data BackboneRouter

-- | Starts the Backbone router
startRouter :: [(Text, [Text] -> Fay ())] -- ^ list of routes and callback handlers, the argument to the callback is the params of the route
            -> Fay BackboneRouter
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
  \ return router;\
\ })()\
\ "

navigate :: Text -- ^ url to navigate
         -> BackboneRouter -- ^ router instance where the url will be handled
         -> Fay ()
navigate = ffi " %2['navigate'](%1, {trigger: true}) "

link :: Text -- ^ label of the a element
     -> Text -- ^ url, that will be navigated to after the user clicks the link
     -> Maybe BackboneRouter -- ^ router instance where the url will be handled, will create links leading nowhere untile the router is initialized
     -> DOMElement
link text route router = let
  aAttr = mkAAttrs {
    href = Defined $ pack "javascript://"
  }
  attr = mkAttrs {
    onClick = case router of 
      Just router' -> Defined $ const $ navigate route router'
      Nothing -> Undefined
  }
  in a'' attr aAttr text
