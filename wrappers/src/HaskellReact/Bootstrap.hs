{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module HaskellReact.Bootstrap where

import HaskellReact (foreignReact, Renderable, CommonJSModule, ReactInstance, Empty (Empty))
import FFI (ffi, Automatic, Defined)
import "fay-base" Data.Text (fromString, Text)

data ReactBootstrap
instance CommonJSModule ReactBootstrap

data ButtonData = ButtonData {
  bsStyle :: Defined Text
  , title :: Defined Text
}

requireReactBootstrap :: ReactBootstrap
requireReactBootstrap = ffi " require('react-bootstrap') "

-- | Creates an instance of a React Bootstrap class
reactBootstrap :: (Renderable b)
               => Text -- ^ The name of the Bootstrap class
               -> Automatic a -- ^ The props passed to the instance
               -> Automatic b -- ^ The children passed to the instance
               -> ReactInstance
reactBootstrap = foreignReact requireReactBootstrap

navBar :: Renderable a
       => Automatic a 
       -> ReactInstance
navBar children = reactBootstrap "Navbar" (Empty {}) children

nav :: Renderable a
    => Automatic a
    -> ReactInstance
nav children = reactBootstrap "Nav" (Empty {}) children
