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
  findInList ,

  checkbox ,
  input ,
  textarea ,
  textInput ,

  row' ,
  row ,
  oneElementRow ,
  buttonRow' ,
  buttonRow ,
  editableRow ,
  inputRow ,
  dropdownRow ,
  nullDropdownRow ) where

import           Prelude                               as P hiding (span, div, elem) 
import           Data.Text                                  (fromString, Text, (<>))
import           FFI                                        (Defined(Defined, Undefined))

import           HaskellReact                               hiding (row, label)
import qualified HaskellReact.Bootstrap.Button         as BTN
import qualified HaskellReact.Tag.Input                as I
import qualified HaskellReact.Bootstrap.ButtonDropdown as BD
import qualified HaskellReact.Tag.Hyperlink            as A

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

findInList :: (Eq a) => Maybe a -> [(a, b)] -> Maybe b
findInList maybeKey list = value where
  lookup' key' = lookup key' list
  value = maybe Nothing lookup' maybeKey


-- form elements

checkbox :: InputState -> Bool -> (Bool -> Fay ()) -> DOMElement
checkbox editing value setter = let
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

input :: InputState -> Bool -> DisplayValue -> (SyntheticEvent -> Fay ()) -> DOMElement
input = textInput I.input

textarea :: InputState -> Bool -> DisplayValue -> (SyntheticEvent -> Fay ()) -> DOMElement
textarea = textInput I.textarea

textInput :: (Attributes -> I.InputAttributes -> DOMElement) 
          -> InputState 
          -> Bool 
          -> DisplayValue 
          -> (SyntheticEvent -> Fay ()) 
          -> DOMElement
textInput mkInput editing' displayPlain displayValue onChange' = let
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
    else mkInput inputNormalAttrs inputAttrs


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
oneElementRow :: (Renderable a, Renderable b)
              => a -- ^ label of field
              -> b -- ^ the other field
              -> DOMElement
oneElementRow formFieldLabel col2 = 
  row formFieldLabel [div' (class' "col-md-9") col2]

buttonRow :: Renderable a
          => a -- ^ label of the button
          -> Fay () -- ^ button on click handler
          -> DOMElement
buttonRow = buttonRow' Enabled

buttonRow' :: Renderable a
           => ButtonState
           -> a -- ^ label of the button
           -> Fay () -- ^ button on click handler
           -> DOMElement
buttonRow' enabled buttonLabel clickHandler = 
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
  input' = input editing' True value' onChange'
  in editableRow editing' labelText input'

-- | Dropdown component
dropdownRow :: InputState 
            -> Text -- label for the row
            -> [(a, b)] -- key value list
            -> (b -> Text) -- format the b value for the user to see
            -> b -- the displayed element in the closed dropdown
            -> (a -> Fay ()) -- selection handler
            -> DOMElement
dropdownRow editing rowLabel elements display currentElement setId = row rowLabel [element] where
  element = case editing of
    Editing -> div' (class' "col-md-9") $ BD.buttonDropdown buttonLabel elementsToBeSelected
    Display -> span' (class'' ["control-label", "col-md-9", "my-text-left"]) $ display currentElement
  selectLink theId label = let
    selectAction = setId theId
    in A.a''' (click selectAction) (display label)
  elementsToBeSelected = map (\(theId, label) -> li $ selectLink theId label) elements
  buttonLabel = [text2DOM $ (display currentElement) <> " " , span' (class' "caret") ""]

-- | Dropdown component with a null value
nullDropdownRow :: InputState
                -> Text 
                -> [(a, b)]
                -> (b -> Text)
                -> Maybe b
                -> (Maybe a -> Fay ())
                -> DOMElement
nullDropdownRow editing rowLabel elements display currentElement setId = 
  dropdownRow editing rowLabel elements' display' currentElement setId where
    elements' = let
      notNullElements = map (\(a,b) -> (Just a, Just b)) elements
      nullElement = (Nothing, Nothing)
      in nullElement : notNullElements
    display' (Just element) = display element
    display' Nothing = "---"
