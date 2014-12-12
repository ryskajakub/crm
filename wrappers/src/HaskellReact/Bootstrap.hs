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

data ButtonProps = ButtonProps {
  bsStyle :: Defined Text
  , title :: Defined Text
  , onClick :: Defined (SyntheticMouseEvent -> Fay ())
}

buttonProps :: ButtonProps
buttonProps = ButtonProps {
  bsStyle = Undefined
  , title = Undefined
  , onClick = Undefined
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
navBar children = reactBootstrap "Navbar" Null children

nav :: Renderable a
    => Automatic a
    -> ReactInstance
nav children = reactBootstrap "Nav" Null children

button' :: Renderable a
        => ButtonProps
        -> Automatic a
        -> ReactInstance
button' props children = reactBootstrap "Button" props children

button :: Renderable a
       => Automatic a
       -> ReactInstance
button = button' buttonProps

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
