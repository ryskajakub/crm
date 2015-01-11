{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HaskellReact.Bootstrap.ButtonDropdown where

import "fay-base" Prelude hiding (div)
import "fay-base" Data.Text (fromString, Text)
import FFI (ffi, Automatic, Defined(Defined, Undefined), Nullable(Null))

import HaskellReact.Event (SyntheticMouseEvent)
import HaskellReact

data ButtonTechnicalProps = ButtonTechnicalProps {
  disabled_ :: Defined Text ,
  type_ :: Text ,
  data_toggle :: Text ,
  aria_expanded :: Text }

data UlTechnicalProps = UlTechnicalProps {
  role :: Text }

buttonDropdown :: Renderable a
               => a
               -> [DOMElement] -- ^ list of li elements rendered in the dropdown list
               -> DOMElement
buttonDropdown = buttonDropdown' True

buttonDropdown' :: Renderable a
                => Bool
                -> a -- ^ button label
                -> [DOMElement] -- ^ list of li elements rendered in the dropdown list
                -> DOMElement
buttonDropdown' enabled buttonLabel liElements = let
  buttonProps = ButtonTechnicalProps {
    data_toggle = "dropdown" ,
    aria_expanded = "false" ,
    type_ = "button" }
  buttonProps' = if enabled
    then buttonProps { disabled_ = Undefined }
    else buttonProps { disabled_ = Defined "disabled" }
  ulProps = UlTechnicalProps {
    role = "menu" }
  in div' (class' "btn-group") [
    constructDOMElement "button" (class'' ["btn","btn-default","dropdown-toggle"]) buttonProps' buttonLabel ,
    constructDOMElement "ul" (class' "dropdown-menu") ulProps liElements ]
