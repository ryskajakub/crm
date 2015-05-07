{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Component.Form where

import "fay-base" Data.Text (fromString, pack, Text)
import "fay-base" Prelude as P hiding (span, div, elem) 
import "fay-base" FFI (Defined(Defined, Undefined))

import HaskellReact
import qualified HaskellReact.Bootstrap.Button as BTN
import qualified HaskellReact.Tag.Input as I
import qualified HaskellReact.Bootstrap.Input as II

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

editable' :: Maybe II.InputProps -- ^ provide the base input props
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
    in inputWrapper $ II.input ((maybe (II.mkInputProps) (P.id) (inputProps)) {
      II.onChange = Defined changeHandler , 
      II.defaultValue = Defined initial })
  else display

editableN :: I.InputAttributes -- ^ element to display in edit mode
          -> Attributes
          -> Bool -- ^ editing mode
          -> DOMElement -- ^ element to display in non-edit mode
          -> DOMElement
editableN inputProps attributes editing display = 
  if editing
  then I.input attributes inputProps
  else display

formRowCol' :: Renderable a
            => Defined Text -- ^ key of the element
            -> a -- ^ label of the label field
            -> [DOMElement] -- ^ other columns
            -> DOMElement
formRowCol' key' formFieldLabel otherColumns =
  div' ((class' "form-group") { key = key' }) [
    (label' (class'' ["control-label", "col-md-3"]) formFieldLabel) : otherColumns]

formRowCol :: Renderable a
           => a -- ^ label of the label field
           -> [DOMElement] -- ^ other columns
           -> DOMElement
formRowCol formFieldLabel otherColumns = formRowCol' Undefined formFieldLabel otherColumns

formRow :: (Renderable a, Renderable b)
        => a -- ^ label of field
        -> b -- ^ the other field
        -> DOMElement
formRow formFieldLabel col2 = 
  formRowCol formFieldLabel [div' (class' "col-md-9") col2]

editingCheckbox :: Bool -> (Bool -> Fay ()) -> Bool -> DOMElement
editingCheckbox value setter editing = let
  disabledAttrs = if editing
    then I.mkInputAttrs
    else I.mkInputAttrs { I.disabled_ = Defined "disabled" }
  checkboxAttrs = disabledAttrs { 
    I.type_ = I.checkbox ,
    I.onChange = Defined $ (eventValue >=> (const $ setter $ not value )) }
  inputAttrs = if value
    then checkboxAttrs { I.checked = Defined "checked" }
    else checkboxAttrs
  in I.input mkAttrs inputAttrs

editingInput :: String -> (SyntheticEvent -> Fay ()) -> Bool -> DOMElement
editingInput = editingInput' False

editingTextarea :: String -> (SyntheticEvent -> Fay ()) -> Bool -> DOMElement
editingTextarea = editingInput' True

editingInput' :: Bool -> String -> (SyntheticEvent -> Fay ()) -> Bool -> DOMElement
editingInput' textarea value' onChange' editing' = let
  inputAttrs = let
    commonInputAttrs = if textarea
      then I.mkInputAttrs
      else I.mkInputAttrs {
        I.defaultValue = Defined $ pack value' }
    in if editing' 
      then commonInputAttrs {
        I.onChange = Defined onChange' }
      else commonInputAttrs { 
        I.disabled_ = Defined "disabled" }
  in if textarea 
    then I.textarea inputNormalAttrs inputAttrs (pack value')
    else I.input inputNormalAttrs inputAttrs

formRow' :: Text -> String -> (SyntheticEvent -> Fay ()) -> Bool -> DOMElement
formRow' labelText value' onChange' editing' = 
  formRow labelText $ editingInput value' onChange' editing' 

saveButtonRow :: Renderable a
              => a -- ^ label of the button
              -> Fay () -- ^ button on click handler
              -> DOMElement
saveButtonRow = saveButtonRow' True

saveButtonRow' :: Renderable a
               => Bool
               -> a -- ^ label of the button
               -> Fay () -- ^ button on click handler
               -> DOMElement
saveButtonRow' enabled buttonLabel clickHandler = 
  div' (class'' ["col-md-9", "col-md-offset-3"]) $
    BTN.button' (let
      buttonProps = (BTN.buttonProps {
        BTN.bsStyle = Defined "primary" ,
        BTN.onClick = Defined $ const clickHandler })
      in if enabled then buttonProps else buttonProps {
        BTN.disabled = Defined True })
      buttonLabel

editDisplayRow :: Renderable a
               => Bool -- ^ editing
               -> Text -- ^ label of field
               -> a -- ^ the other field
               -> DOMElement
editDisplayRow editing labelText otherField = let
  classes = ["col-md-9", "my-text-left"] ++ (if editing
    then []
    else ["control-label"])
  in formRowCol labelText [div' (class'' classes) otherField]

inputNormalAttrs :: Attributes
inputNormalAttrs = class' "form-control"

row' :: Bool -> Text -> [Char] -> (SyntheticEvent -> Fay ()) -> DOMElement
row' editing' labelText value' onChange' = let
  inputAttrs = I.mkInputAttrs {
    I.defaultValue = Defined $ pack value' ,
    I.onChange = Defined onChange' }
  input = editableN inputAttrs inputNormalAttrs editing' (
    span $ pack value')
  in editDisplayRow editing' labelText input
