module HaskellReact.Router where

import HaskellReact (foreignReact, Renderable, CommonJSModule, ReactClass, ReactInstance)
import FFI (ffi, Automatic)

requireReactRouter :: ReactRouter
requireReactRouter = ffi " require('react-router') "

reactRouter :: Renderable b
            => String -- ^ The name of the ReactRouter class
            -> Automatic a -- ^ Props that will be passed to the instance
            -> Automatic b -- ^ The children passed to the instance
            -> ReactInstance
reactRouter = foreignReact requireReactRouter

data ReactRouter
instance CommonJSModule ReactRouter

data RouteData a = RouteData {
  path :: String
  , handler :: ReactClass a
}

runRouter :: ReactInstance -- root of the routes
          -> Fay ()
runRouter = ffi "\
  \ (function () {\
    \ var React = require('react');\
    \ require('react-router')['run'](%1, function(Handler) {\
      \ React.render(React.createElement(Handler, null), document.body);\
    \ });\
  \ })()\
  \ "
