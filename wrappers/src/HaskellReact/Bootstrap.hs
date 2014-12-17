{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HaskellReact.Bootstrap where

import HaskellReact (foreignReact, Renderable, CommonJSModule, ReactInstance, Empty (Empty))
import FFI (ffi, Automatic, Defined(Undefined), Nullable(Null))
import "fay-base" Data.Text (fromString, Text)
import HaskellReact.Event (SyntheticMouseEvent)
import "fay-base" Prelude

data ReactBootstrap
instance CommonJSModule ReactBootstrap

requireReactBootstrap :: ReactBootstrap
requireReactBootstrap = ffi " require('react-bootstrap') "

-- | Creates an instance of a React Bootstrap class
reactBootstrap :: (Renderable b)
               => Text -- ^ The name of the Bootstrap class
               -> a -- ^ The props passed to the instance
               -> b -- ^ The children passed to the instance
               -> ReactInstance
reactBootstrap = foreignReact requireReactBootstrap

navBar :: Renderable a
       => Automatic a 
       -> ReactInstance
navBar children = reactBootstrap "Navbar" Null children

nav :: Renderable a
    => Automatic a
    -> ReactInstance
nav children = reactBootstrap "Nav" Null children

table :: Renderable a
      => Automatic a
      -> ReactInstance
table = reactBootstrap "Table" Null

row :: Renderable a
    => Automatic a
    -> ReactInstance
row = reactBootstrap "Row" Null

jumbotron :: Renderable a
          => Automatic a
          -> ReactInstance
jumbotron = reactBootstrap "Jumbotron" Null

grid :: Renderable a
     => Automatic a
     -> ReactInstance
grid = reactBootstrap "Grid" Null

data ColProps = ColProps {
  md :: Int
}

col :: Renderable a
    => ColProps
    -> Automatic a
    -> ReactInstance
col colProps = reactBootstrap "Col" colProps

panel :: Renderable a
      => Automatic a
      -> ReactInstance
panel = reactBootstrap "Panel" Null
