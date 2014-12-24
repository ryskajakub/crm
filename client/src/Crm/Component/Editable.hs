{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Component.Editable (
  editable, editable',
  editableN ) where

import "fay-base" Prelude as P
import "fay-base" Data.Text (Text)
import FFI (Defined (Defined))

import HaskellReact as HR
import qualified HaskellReact.Bootstrap.Input as I
import qualified HaskellReact.Tag.Input as II

editable :: Bool -- ^ edit state
         -> DOMElement -- ^ display value
         -> Text -- ^ initial value to display in the input
         -> (Text -> Fay ()) -- ^ callback to call when the input field is changed due to user typing
         -> DOMElement -- ^ either input field or displayed value depending on editing parameter
editable = editable' Nothing

editable' :: Maybe I.InputProps -- ^ provide the base input props
          -> Bool -- ^ edit state
          -> DOMElement -- ^ display value
          -> Text -- ^ initial value to display in the input
          -> (Text -> Fay ()) -- ^ callback to call when the input field is changed due to user typing
          -> DOMElement -- ^ either input field or displayed value depending on editing parameter
editable' inputProps edit display initial setValue = if edit
  then let
    changeHandler event = do
      value <- eventValue event
      setValue value
    in I.input ((maybe (I.mkInputProps) (P.id) (inputProps)) {
      I.onChange = Defined changeHandler , 
      I.defaultValue = Defined initial })
  else display

editableN :: II.InputAttributes -- ^ element to display in edit mode
          -> HR.Attributes
          -> Bool -- ^ editing mode
          -> DOMElement -- ^ element to display in non-edit mode
          -> DOMElement
editableN inputProps attributes editing display = 
  if editing
  then II.input attributes inputProps
  else display
