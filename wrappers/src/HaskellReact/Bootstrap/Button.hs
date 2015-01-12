{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HaskellReact.Bootstrap.Button where

import "fay-base" Prelude
import "fay-base" Data.Text (fromString, Text)
import "fay-base" FFI (Defined(Undefined))

import HaskellReact (Renderable, DOMElement)
import HaskellReact.Event (SyntheticMouseEvent)
import HaskellReact.Bootstrap

data ButtonProps = ButtonProps {
  bsStyle :: Defined Text , 
  title :: Defined Text , 
  onClick :: Defined (SyntheticMouseEvent -> Fay ()) ,
  disabled :: Defined Bool }

buttonProps :: ButtonProps
buttonProps = ButtonProps {
  bsStyle = Undefined ,
  title = Undefined ,
  disabled = Undefined ,
  onClick = Undefined }

button' :: Renderable a
        => ButtonProps
        -> a
        -> DOMElement
button' props children = reactBootstrap "Button" props children

button :: Renderable a
       => a
       -> DOMElement
button = button' buttonProps
