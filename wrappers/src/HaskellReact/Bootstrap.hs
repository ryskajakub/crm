{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HaskellReact.Bootstrap where

import HaskellReact (foreignReact, Renderable, CommonJSModule, ReactInstance, Empty (Empty))
import FFI (ffi, Automatic, Defined(Undefined))
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
navBar children = reactBootstrap "Navbar" (Empty {}) children

nav :: Renderable a
    => Automatic a
    -> ReactInstance
nav children = reactBootstrap "Nav" (Empty {}) children

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
table = reactBootstrap "Table" (Empty {})
