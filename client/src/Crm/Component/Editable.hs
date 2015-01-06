{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Component.Editable (
  editable, editable',
  editablePlain ,
  editableN ) where

import "fay-base" Prelude as P
import "fay-base" Data.Text (Text)
import FFI (Defined (Defined))

import HaskellReact as HR
import qualified HaskellReact.Bootstrap.Input as I
import qualified HaskellReact.Tag.Input as II

editablePlain :: Bool
              -> Text
              -> (Text -> Fay())
              -> DOMElement
editablePlain editState displayValue =
  editable editState (text2DOM displayValue) displayValue

editable :: Bool -- ^ edit state
         -> DOMElement -- ^ display value
         -> Text -- ^ initial value to display in the input
         -> (Text -> Fay ()) -- ^ callback to call when the input field is changed due to user typing
         -> DOMElement -- ^ either input field or displayed value depending on editing parameter
editable = editable' Nothing P.id

editable' :: Maybe I.InputProps -- ^ provide the base input props
          -> (DOMElement -> DOMElement) -- ^ wrapper of the input field in the edit mode
          -> Bool -- ^ edit state
          -> DOMElement -- ^ display value
          -> Text -- ^ initial value to display in the input
          -> (Text -> Fay ()) -- ^ callback to call when the input field is changed due to user typing
          -> DOMElement -- ^ either input field or displayed value depending on editing parameter
editable' inputProps inputWrapper edit display initial setValue = if edit
  then let
    changeHandler event = do
      value <- eventValue event
      setValue value
    in inputWrapper $ I.input ((maybe (I.mkInputProps) (P.id) (inputProps)) {
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
