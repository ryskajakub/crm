{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ExistentialQuantification #-}

module HaskellReact.Router where

import HaskellReact (foreignReact, Renderable, CommonJSModule, ReactClass, ReactInstance, DOMElement, Empty (Empty))
import "fay-base" Data.Text (Text)
import FFI (ffi, Automatic, Defined(Defined, Undefined))

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

data RouteData = forall a. RouteData {
  path :: Defined String
  , handler :: ReactClass a
  , name :: Defined Text
}

routeProps :: Text -> ReactClass a -> Defined String -> RouteData
routeProps name handler path = RouteData path handler (Defined name)

newtype DefaultRouteProps = DefaultRouteProps { routeData :: RouteData }

defaultRouteProps :: Defined Text -> ReactClass a -> DefaultRouteProps
defaultRouteProps name handler = DefaultRouteProps $ RouteData Undefined handler name

route :: Renderable a
      => RouteData -- ^ Props that will be passed to the instance
      -> Automatic a -- ^ The children passed to the instance
      -> ReactInstance
route = reactRouter "Route"

defaultRoute :: DefaultRouteProps
             -> ReactInstance
defaultRoute routeData = reactRouter "DefaultRoute" routeData ([] :: [DOMElement])

routeHandler :: ReactInstance
routeHandler = reactRouter "RouteHandler" (Empty {}) ([] :: [DOMElement])

runRouter :: ReactInstance -- ^ Root of the routes
          -> Fay ()
runRouter = ffi "\
  \ (function () {\
    \ var React = require('react');\
    \ var Router = require('react-router');\
    \ Router['run'](%1, function(Handler) {\
      \ React.render(React.createElement(Handler), document.body);\
    \ });\
  \ })()\
  \ "

data LinkProps = LinkProps {
  to :: Text
}

link :: Renderable b
     => LinkProps
     -> Automatic b -- ^ Children passed to instance
     -> ReactInstance
link linkProps children = reactRouter "Link" linkProps children
