{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Component.Editable (
  editable
) where

import HaskellReact as HR
import "fay-base" Prelude
import "fay-base" Data.Text (Text)
import qualified HaskellReact.Bootstrap.Input as I
import FFI (Defined (Defined))

editable :: Bool -- ^ edit state
         -> DOMElement -- ^ display value
         -> Text -- ^ initial value to display in the input
         -> (Text -> Fay ()) -- ^ callback to call when the input field is changed due to user typing
         -> DOMElement -- ^ either input field or displayed value depending on editing parameter
editable edit display initial setValue = if edit
  then let
    changeHandler event = do
      value <- eventValue event
      setValue value
    in I.input (I.mkInputProps {
      I.onChange = Defined changeHandler
      , I.defaultValue = initial
      })
  else display
