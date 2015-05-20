{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Crm.Component.Form where

import Data.Text (fromString, Text, (<>))
import Prelude as P hiding (span, div, elem) 
import FFI (Defined(Defined, Undefined))

import HaskellReact
import qualified HaskellReact.Bootstrap.Button as BTN
import qualified HaskellReact.Tag.Input as I
import qualified HaskellReact.Bootstrap.Input as II
import qualified HaskellReact.Bootstrap.ButtonDropdown as BD
import qualified HaskellReact.Tag.Hyperlink as A

import Crm.Helpers (lmap)

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

data DisplayValue = DefaultValue Text | SetValue Text

joinEither :: DisplayValue -> Text
joinEither dv = case dv of
  DefaultValue x -> x
  SetValue x -> x

editingInput :: Bool -> DisplayValue -> (SyntheticEvent -> Fay ()) -> Bool -> DOMElement
editingInput = editingInput' False

editingTextarea :: Bool -> DisplayValue -> (SyntheticEvent -> Fay ()) -> Bool -> DOMElement
editingTextarea = editingInput' True

editingInput' :: Bool -> Bool -> DisplayValue -> (SyntheticEvent -> Fay ()) -> Bool -> DOMElement
editingInput' textarea displayPlain displayValue onChange' editing' = let
  inputAttrs = let
    commonInputAttrs = case displayValue of
      DefaultValue t -> I.mkInputAttrs { I.defaultValue = Defined t }
      SetValue t -> I.mkInputAttrs { I.value_ = Defined t }
    in if editing' 
      then commonInputAttrs {
        I.onChange = Defined onChange' }
      else commonInputAttrs { 
        I.disabled_ = Defined "disabled" }
  in if displayPlain && not editing'
    then text2DOM $ joinEither displayValue
    else if textarea 
      then I.textarea inputNormalAttrs inputAttrs
      else I.input inputNormalAttrs inputAttrs

formRow' :: Text -> DisplayValue -> (SyntheticEvent -> Fay ()) -> Bool -> DOMElement
formRow' labelText value' onChange' editing' = 
  formRow labelText $ editingInput True value' onChange' editing' 

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

row' :: Bool -> Text -> DisplayValue -> (SyntheticEvent -> Fay ()) -> DOMElement
row' editing' labelText value' onChange' = let
  input = editingInput True value' onChange' editing'
  in editDisplayRow editing' labelText input

maybeSelectRow' :: (Eq a) 
                => Bool
                -> Bool 
                -> Text 
                -> [(a, b)] 
                -> (b -> Text) 
                -> Maybe a 
                -> (Maybe a -> Fay ()) 
                -> (Text -> b) 
                -> DOMElement
maybeSelectRow' displayNoElement editing rowLabel elements getLabel theId' setId emptyRecord =
  formRowCol rowLabel [let
    noElementSelectedLabel = "---"
    selectedLabel = maybe noElementSelectedLabel (\theId -> let
      elementFound = lookup theId elements
      in maybe noElementSelectedLabel getLabel elementFound) theId'
    selectLink theId record = let
      selectAction = setId theId
      in A.a''' (click selectAction) (getLabel record)
    noElement = if displayNoElement then [(Nothing, emptyRecord noElementSelectedLabel)] else []
    withEmptyRecord = noElement ++ (map (lmap Just) elements)
    elementsToBeSelected = map (\(theId, record) -> li $ selectLink theId record) withEmptyRecord
    buttonLabel = [ text2DOM $ selectedLabel <> " " , span' (class' "caret") "" ]
    in if editing
      then div' (class' "col-md-9") $ BD.buttonDropdown buttonLabel elementsToBeSelected
      else span' (class'' ["control-label", "col-md-9", "my-text-left"]) selectedLabel ]

maybeSelectRow :: (Eq a) 
               => Bool
               -> Text 
               -> [(a, b)] 
               -> (b -> Text) 
               -> Maybe a 
               -> (Maybe a -> Fay ()) 
               -> (Text -> b) 
               -> DOMElement
maybeSelectRow = maybeSelectRow' True
