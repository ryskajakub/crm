{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Crm.Component.Form (
  InputState (..) ,
  ButtonState (..) ,
  DisplayValue (..) ,

  buttonStateFromBool ,
  inputStateFromBool ,
  inputStateToBool ,
  inputNormalAttrs ,

  editingCheckbox ,
  editingInput ,
  editingTextarea ,
  editingInput' ,

  row' ,
  row ,
  rowOneElement ,
  saveButtonRow' ,
  saveButtonRow ,
  editableRow ,
  inputRow ,
  maybeSelectRow' ,
  maybeSelectRow 
  ) where

import           Prelude                               as P hiding (span, div, elem) 
import           Data.Text                                  (fromString, Text, (<>))
import           FFI                                        (Defined(Defined, Undefined))

import           HaskellReact                               hiding (row)
import qualified HaskellReact.Bootstrap.Button         as BTN
import qualified HaskellReact.Tag.Input                as I
import qualified HaskellReact.Bootstrap.Input          as II
import qualified HaskellReact.Bootstrap.ButtonDropdown as BD
import qualified HaskellReact.Tag.Hyperlink            as A

import Crm.Helpers (lmap)

data InputState = Editing | Display
  deriving Eq

data ButtonState = Enabled | Disabled
  deriving Eq

data DisplayValue = DefaultValue Text | SetValue Text
  deriving Eq


-- utilities

buttonStateFromBool :: Bool -> ButtonState
buttonStateFromBool True = Enabled
buttonStateFromBool False = Disabled

inputStateFromBool :: Bool -> InputState
inputStateFromBool True = Editing
inputStateFromBool False = Display

inputStateToBool :: InputState -> Bool
inputStateToBool Editing = True
inputStateToBool Display = False

joinEither :: DisplayValue -> Text
joinEither dv = case dv of
  DefaultValue x -> x
  SetValue x -> x

inputNormalAttrs :: Attributes
inputNormalAttrs = class' "form-control"


-- form elements

editingCheckbox :: InputState -> Bool -> (Bool -> Fay ()) -> DOMElement
editingCheckbox editing value setter = let
  disabledAttrs = case editing of
    Editing -> I.mkInputAttrs
    _ -> I.mkInputAttrs { I.disabled_ = Defined "disabled" }
  checkboxAttrs = disabledAttrs { 
    I.type_ = I.checkbox ,
    I.onChange = Defined $ (eventValue >=> (const $ setter $ not value )) }
  inputAttrs = if value
    then checkboxAttrs { I.checked = Defined "checked" }
    else checkboxAttrs
  in I.input mkAttrs inputAttrs

editingInput :: InputState -> Bool -> DisplayValue -> (SyntheticEvent -> Fay ()) -> DOMElement
editingInput = editingInput' False

editingTextarea :: InputState -> Bool -> DisplayValue -> (SyntheticEvent -> Fay ()) -> DOMElement
editingTextarea = editingInput' True

editingInput' :: Bool -> InputState -> Bool -> DisplayValue -> (SyntheticEvent -> Fay ()) -> DOMElement
editingInput' textarea editing' displayPlain displayValue onChange' = let
  inputAttrs = let
    commonInputAttrs = case displayValue of
      DefaultValue t -> I.mkInputAttrs { I.defaultValue = Defined t }
      SetValue t -> I.mkInputAttrs { I.value_ = Defined t }
    in case editing' of
      Editing -> commonInputAttrs {
        I.onChange = Defined onChange' }
      _ -> commonInputAttrs { 
        I.disabled_ = Defined "disabled" }
  in if displayPlain && (editing' == Display)
    then text2DOM $ joinEither displayValue
    else if textarea 
      then I.textarea inputNormalAttrs inputAttrs
      else I.input inputNormalAttrs inputAttrs


-- row elements

row' :: Renderable a
     => Defined Text -- ^ key of the element
     -> a -- ^ label of the label field
     -> [DOMElement] -- ^ other columns
     -> DOMElement
row' key' formFieldLabel otherColumns =
  div' ((class' "form-group") { key = key' }) [
    (label' (class'' ["control-label", "col-md-3"]) formFieldLabel) : otherColumns]

row :: Renderable a
    => a -- ^ label of the label field
    -> [DOMElement] -- ^ other columns
    -> DOMElement
row formFieldLabel otherColumns = row' Undefined formFieldLabel otherColumns

-- | Row containing a label and another element in ratio of size 1:3
rowOneElement :: (Renderable a, Renderable b)
              => a -- ^ label of field
              -> b -- ^ the other field
              -> DOMElement
rowOneElement formFieldLabel col2 = 
  row formFieldLabel [div' (class' "col-md-9") col2]

saveButtonRow :: Renderable a
              => a -- ^ label of the button
              -> Fay () -- ^ button on click handler
              -> DOMElement
saveButtonRow = saveButtonRow' Enabled

saveButtonRow' :: Renderable a
               => ButtonState
               -> a -- ^ label of the button
               -> Fay () -- ^ button on click handler
               -> DOMElement
saveButtonRow' enabled buttonLabel clickHandler = 
  div' (class'' ["col-md-9", "col-md-offset-3"]) $
    BTN.button' (let
      buttonProps = (BTN.buttonProps {
        BTN.bsStyle = Defined "primary" ,
        BTN.onClick = Defined $ const clickHandler })
      in case enabled of
        Enabled -> buttonProps 
        _ -> buttonProps { BTN.disabled = Defined True })
      buttonLabel

-- | Row that has two modes, editing and display each having different css classes for different display
editableRow :: (Renderable a, Renderable b)
            => InputState -- ^ editing
            -> a -- ^ label of field
            -> b -- ^ the other field
            -> DOMElement
editableRow editing labelText otherField = let
  classes = "col-md-9" : case editing of
    Editing -> []
    Display -> ["control-label", "my-text-left"]
  in row labelText [div' (class'' classes) otherField]

-- | Row having an input field in editing mode, just display in the display mode
inputRow :: InputState -- ^ editing/display mode
         -> Text -- ^ label to display on the left of the input
         -> DisplayValue -- ^ value to display or to set in the form
         -> (SyntheticEvent -> Fay ()) -- ^ event to handle on input change
         -> DOMElement -- ^ rendered element
inputRow editing' labelText value' onChange' = let
  input = editingInput editing' True value' onChange'
  in editableRow editing' labelText input

maybeSelectRow' :: (Eq a) 
                => Bool
                -> InputState 
                -> Text 
                -> [(a, b)] 
                -> (b -> Text) 
                -> Maybe a 
                -> (Maybe a -> Fay ()) 
                -> (Text -> b) 
                -> DOMElement
maybeSelectRow' displayNoElement editing rowLabel elements getLabel theId' setId emptyRecord =
  row rowLabel [let
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
    in case editing of
      Editing -> div' (class' "col-md-9") $ BD.buttonDropdown buttonLabel elementsToBeSelected
      Display -> span' (class'' ["control-label", "col-md-9", "my-text-left"]) selectedLabel ]

maybeSelectRow :: (Eq a) 
               => InputState
               -> Text 
               -> [(a, b)] 
               -> (b -> Text) 
               -> Maybe a 
               -> (Maybe a -> Fay ()) 
               -> (Text -> b) 
               -> DOMElement
maybeSelectRow = maybeSelectRow' True
