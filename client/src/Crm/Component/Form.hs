{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}

module Crm.Component.Form (
  InputState (..) ,
  ButtonState (..) ,
  DisplayValue (..) ,
  OrderingControls (..) ,
  
  buttonStateFromBool ,
  inputStateFromBool ,
  inputStateToBool ,
  inputNormalAttrs ,
  findInList ,

  checkbox ,
  input ,
  textarea ,
  textInput ,
  dropdown ,
  nullDropdown ,

  row' ,
  row ,
  oneElementRow ,
  buttonRow' ,
  buttonRow ,
  editableRow ,
  inputRow ,
  textareaRow ,
  dropdownRow ,
  nullDropdownRow ,
  multipleInputs ) where

import           Prelude                               as P hiding (span, div, elem) 
import           Data.Text                             (fromString, Text, (<>), showInt)
import           FFI                                   (Defined(Defined, Undefined))

import           Crm.Helpers                           (zipWithIndex)

import           HaskellReact                          hiding (row, label)
import qualified HaskellReact.Bootstrap.Button         as BTN
import qualified HaskellReact.Bootstrap                as B
import qualified HaskellReact.Tag.Input                as I
import qualified HaskellReact.Bootstrap.ButtonDropdown as BD
import qualified HaskellReact.Tag.Hyperlink            as A
import qualified HaskellReact.Bootstrap.Glyphicon      as G


-- datas

data InputState = Editing | Display
  deriving Eq

data ButtonState = Enabled | Disabled
  deriving Eq

data DisplayValue = DefaultValue Text | SetValue Text
  deriving Eq

data FieldPosition = First | Last | Single | Middle

data OrderingControls = OrderingVisible | OrderingInvisible


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

input :: InputState -> Bool -> DisplayValue -> (Text -> Fay ()) -> DOMElement
input = textInput I.input

textarea :: InputState -> Bool -> DisplayValue -> (Text -> Fay ()) -> DOMElement
textarea = textInput I.textarea

textInput :: (Attributes -> I.InputAttributes -> DOMElement) 
          -> InputState
          -> Bool 
          -> DisplayValue 
          -> (Text -> Fay ()) 
          -> DOMElement
textInput mkInput editing' displayPlain displayValue onChange' = let
  inputAttrs = let
    commonInputAttrs = case displayValue of
      DefaultValue t -> I.mkInputAttrs { I.defaultValue = Defined t }
      SetValue t -> I.mkInputAttrs { I.value_ = Defined t }
    in case editing' of
      Editing -> commonInputAttrs {
        I.onChange = Defined $ eventValue >=> onChange' }
      _ -> commonInputAttrs { 
        I.disabled_ = Defined "disabled" }
  in if displayPlain && (editing' == Display)
    then text2DOM $ joinEither displayValue
    else mkInput inputNormalAttrs inputAttrs

dropdown :: [(a, b)] -- key value list
         -> (b -> Text) -- format the b value for the user to see
         -> b -- the displayed element in the closed dropdown
         -> (a -> Fay ()) -- selection handler
         -> DOMElement
dropdown elements display currentElement setId = element where
  element = BD.buttonDropdown buttonLabel elementsToBeSelected
  selectLink theId label = let
    selectAction = setId theId
    in A.a''' (click selectAction) (display label)
  elementsToBeSelected = map (\(theId, label) -> li $ selectLink theId label) elements
  buttonLabel = [text2DOM $ (display currentElement) <> " " , span' (class' "caret") ""]

nullDropdown :: [(a, b)]
             -> (b -> Text)
             -> Maybe b
             -> (Maybe a -> Fay ())
             -> DOMElement
nullDropdown elements display currentElement setId = 
  dropdown elements' display' currentElement setId where
    elements' = let
      notNullElements = map (\(a,b) -> (Just a, Just b)) elements
      nullElement = (Nothing, Nothing)
      in nullElement : notNullElements
    display' (Just element) = display element
    display' Nothing = "---"


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
         -> (Text -> Fay ()) -- ^ event to handle on input change
         -> DOMElement -- ^ rendered element
inputRow editing' labelText value' onChange' = let
  input' = input editing' True value' onChange'
  in editableRow editing' labelText input'

-- | Similar to inputRow, only renders textarea
textareaRow :: InputState
            -> Text
            -> DisplayValue
            -> (Text -> Fay ())
            -> DOMElement
textareaRow editing label value onChange = editableRow editing label textarea' where
  textarea' = textarea editing True value onChange

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


multipleInputs :: forall a.
                  Text
               -> Text
               -> OrderingControls
               -> ([a] -> Fay ())
               -> (a -> (a -> Fay ()) -> DOMElement) -- | the inputlike element
               -> [a] 
               -> a
               -> [DOMElement]
multipleInputs fieldLabel' addNewButtonLabel orderingControlsFlag setList inputControl elems newField = 

  map (displayRow . assignPosition) (zipWithIndex elems) ++ [addAnotherFieldRow] where

    (includeOrderingControls, labelFieldSize) = case orderingControlsFlag of
      OrderingVisible   -> ((:[]), 2)
      OrderingInvisible -> (const [], 3)

    addAnotherFieldRow = oneElementRow addAnotherFieldButton ""
    addAnotherFieldButton = let
      addField = let
        newList = elems ++ [newField]
        in setList newList
      props = BTN.buttonProps { BTN.onClick = Defined $ const addField }
      in BTN.button' props addNewButtonLabel

    displayRow (index, positionInOrdering, a) = mkRow where
      mkRow = mkRowMarkup $
        includeOrderingControls orderingControls ++ [
        fieldLabel ,
        input' ,
        removeButton ]
      mkRowMarkup = div' ((class' "form-group") { key = Defined $ "key-" <> showInt index })
      orderingControls = div' (class'' ["col-md-1", "control-label"]) $ downArrow ++ upArrow where
        changeOrder :: Bool -> [a]
        changeOrder down = let
          (start, (y:x:rest)) = splitAt (if down then index else index - 1) elems
          in start ++ (x:y:rest)
        downArrowLink = A.a''' (click . setList $ changeOrder True) G.arrowDown
        downArrow = case positionInOrdering of
          Middle -> [downArrowLink]
          First -> [downArrowLink]
          _ -> []
        upArrowLink = A.a''' (click . setList $ changeOrder False) G.arrowUp 
        upArrow = case positionInOrdering of
          Middle -> [upArrowLink]
          Last -> [upArrowLink]
          _ -> []
      fieldLabel = label' 
        (class'' ["control-label", "col-md-" <> showInt labelFieldSize])
        (fieldLabel' <> " " <> showInt index)
      setFieldValue a' = let
        (start, _ : rest) = splitAt index elems
        in setList $ start ++ [a'] ++ rest
      input' = div' (class' "col-md-7") $ inputControl a (setFieldValue)
      removeButton = let
        removeField = let
          (start, _:rest) = splitAt index elems
          newFields = start ++ rest
          in setList newFields
        props = BTN.buttonProps {
          BTN.bsStyle = Defined "danger" ,
          BTN.onClick = Defined $ const removeField }
        buttonLabel = "Odeber"
        button = BTN.button' props buttonLabel
        in B.col (B.mkColProps 2) button

    assignPosition (i, field) = if 
      | i == 0 && i == lastIndex -> (i, Single, field)
      | i == lastIndex -> (i, Last, field)
      | i == 0 -> (i, First, field)
      | True -> (i, Middle, field)
      where
      lastIndex = length elems - 1
