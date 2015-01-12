{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HaskellReact.Bootstrap where

import "fay-base" FFI (ffi, Automatic, Defined(Undefined), Nullable(Null))
import "fay-base" Data.Text (fromString, Text)
import "fay-base" Prelude

import HaskellReact (foreignReact, Renderable, CommonJSModule, DOMElement)

data ReactBootstrap
instance CommonJSModule ReactBootstrap

requireReactBootstrap :: ReactBootstrap
requireReactBootstrap = ffi " require('react-bootstrap') "

-- | Creates an instance of a React Bootstrap class
reactBootstrap :: (Renderable b)
               => Text -- ^ The name of the Bootstrap class
               -> a -- ^ The props passed to the instance
               -> b -- ^ The children passed to the instance
               -> DOMElement
reactBootstrap = foreignReact requireReactBootstrap

navBar :: Renderable a
       => Automatic a 
       -> DOMElement
navBar children = reactBootstrap "Navbar" Null children

nav :: Renderable a
    => Automatic a
    -> DOMElement
nav children = reactBootstrap "Nav" Null children

table :: Renderable a
      => Automatic a
      -> DOMElement
table = reactBootstrap "Table" Null

row' :: Renderable b
     => a -- ^ props
     -> b -- ^ children
     -> DOMElement
row' = reactBootstrap "Row" 

row :: Renderable a
    => a
    -> DOMElement
row = row' Null

jumbotron :: Renderable a
          => Automatic a
          -> DOMElement
jumbotron = reactBootstrap "Jumbotron" Null

grid :: Renderable a
     => Automatic a
     -> DOMElement
grid = reactBootstrap "Grid" Null

data ColProps = ColProps {
  md :: Int , 
  mdOffset :: Defined Int }

mkColProps :: Int -> ColProps
mkColProps int = ColProps int Undefined

col :: Renderable a
    => ColProps
    -> Automatic a
    -> DOMElement
col colProps = reactBootstrap "Col" colProps

panel :: Renderable a
      => Automatic a
      -> DOMElement
panel = reactBootstrap "Panel" Null
