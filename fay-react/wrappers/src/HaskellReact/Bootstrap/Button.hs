{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HaskellReact.Bootstrap.Button where

import "fay-base" Prelude
import "fay-base" Data.Text (fromString, Text, (<>))
import "fay-base" FFI (Defined(Undefined, Defined))

import qualified HaskellReact as HR
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

data ExtraButtonProps = ExtraButtonProps {
  data_toggle :: Defined Text ,
  data_target :: Defined Text }

extraButtonProps :: ExtraButtonProps
extraButtonProps = ExtraButtonProps {
  data_toggle = Undefined ,
  data_target = Undefined }

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
buttonP = buttonP' extraButtonProps

buttonP' :: 
  HR.Renderable a =>
  ExtraButtonProps ->
  ButtonSize ->
  ButtonStyle ->
  (SyntheticMouseEvent -> Fay ()) ->
  a ->
  HR.DOMElement
buttonP' extraProps buttonSize buttonStyle clickHandler title' =
  HR.constructDOMElement "button" buttonAttributes extraProps title'
  where
  buttonAttributes = (HR.class'' ["btn", "btn-" <> expressSize buttonSize, "btn-" <> expressStyle buttonStyle]) {
    HR.onClick = Defined clickHandler }
