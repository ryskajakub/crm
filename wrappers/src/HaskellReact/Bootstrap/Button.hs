{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HaskellReact.Bootstrap.Button where

import "fay-base" Prelude
import "fay-base" Data.Text (fromString, Text, (<>))
import "fay-base" FFI (Defined(Undefined, Defined))

import qualified HaskellReact as HR
import qualified HaskellReact.Tag.Controls as HC
import HaskellReact.Event (SyntheticMouseEvent)
import HaskellReact.Bootstrap (reactBootstrap)

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

button' :: 
  HR.Renderable a => 
  ButtonProps -> 
  a -> 
  HR.DOMElement
button' props children = reactBootstrap "Button" props children

button :: 
  HR.Renderable a => 
  a -> 
  HR.DOMElement
button = button' buttonProps

data ButtonStyle = PrimaryButton | DefaultButton

data ButtonSize = LargeButton | NormalButton | SmallButton | ExtraSmallButton

expressStyle :: ButtonStyle -> Text
expressStyle PrimaryButton = "primary"
expressStyle DefaultButton = "default"

expressSize :: ButtonSize -> Text
expressSize LargeButton = "lg"
expressSize NormalButton = ""
expressSize SmallButton = "sm"
expressSize ExtraSmallButton = "xs"

buttonP :: 
  HR.Renderable a =>
  ButtonSize ->
  ButtonStyle ->
  (SyntheticMouseEvent -> Fay ()) ->
  a ->
  HR.DOMElement
buttonP buttonSize buttonStyle clickHandler title =
  HC.button' buttonAttributes title 
  where
  buttonAttributes = (HR.class'' ["btn", "btn-" <> expressSize buttonSize, "btn-" <> expressStyle buttonStyle]) {
    HR.onClick = Defined clickHandler }


