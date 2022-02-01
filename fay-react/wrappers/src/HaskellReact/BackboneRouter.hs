{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HaskellReact.BackboneRouter where

import "fay-base" Data.Text (Text, pack, (<>))
import "fay-base" Prelude
import "fay-base" FFI (ffi, Defined(Defined), Automatic)

import HaskellReact
import HaskellReact.Tag.Hyperlink (mkAAttrs, href, a'')

data BackboneRouter

-- | Starts the Backbone router
startRouter :: [(Text, BackboneRouter -> [Text] -> Fay ())] -- ^ list of routes and callback handlers, the argument to the callback is the params of the route and the router itself
            -> Fay BackboneRouter
startRouter = ffi " \
\ (function () {\
  \ var Backbone = require('backbone');\
  \ var routesFromFay = %1;\
  \ var routes = {}; \
  \ var router = new Backbone.Router(); \
  \ for (var i = 0; i < routesFromFay.length; i++){\
    \ router.route(routesFromFay[i][0], (function(i) { return function() { routesFromFay[i][1](router, arguments); }; })(i)); \
  \ }\
  \ Backbone.history.start();\
  \ return router;\
\ })()\
\ "

refresh :: Fay ()
refresh = ffi " require('backbone').history.loadUrl(require('backbone').history.fragment) "

navigate :: Text -- ^ url to navigate
         -> BackboneRouter -- ^ router instance where the url will be handled
         -> Fay ()
navigate = ffi " %2['navigate'](%1, {trigger: true}) "

link :: Renderable a
     => Automatic a -- ^ children
     -> Text -- ^ url, that will be navigated to after the user clicks the link
     -> BackboneRouter -- ^ router instance where the url will be handled, will create links leading nowhere untile the router is initialized
     -> DOMElement
link children route _ = let
  aAttr = mkAAttrs {
    href = Defined $ pack "/#" <> route }
  attr = mkAttrs 
  in a'' attr aAttr children
