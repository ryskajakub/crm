{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Helpers where

import "fay-base" Data.Text (Text, showInt, pack, fromString)
import "fay-base" Prelude hiding (div, span, id)
import FFI (Nullable, ffi, Defined (Defined))
import Data.Nullable (fromNullable)

import qualified Crm.Shared.YearMonthDay as YMD
import qualified Crm.Shared.Company as C

import HaskellReact
import qualified HaskellReact.Bootstrap.CalendarInput as CI
import qualified HaskellReact.Bootstrap.Button as BTN
import qualified HaskellReact.Tag.Input as I

import Moment

parseInt :: Text -> Nullable Int
parseInt = ffi " (function() { var int = parseInt(%1); ret = ((typeof int) === 'number' && !isNaN(int)) ? int : null; return ret; })() "

parseSafely :: Text -> Maybe Int
parseSafely possibleNumber = fromNullable $ parseInt possibleNumber

displayDate :: YMD.YearMonthDay -> Text
displayDate (YMD.YearMonthDay y m d precision) = let
  dateAsMoment = dayPrecision y m d requireMoment
  in format dateAsMoment (case precision of
    YMD.MonthPrecision -> "MMMM YYYY"
    _ -> "LL")

showCompanyId :: C.CompanyId -> Text
showCompanyId = showInt . C.getCompanyId

displayPrecision :: YMD.Precision -> CI.DisplayDatePrecision
displayPrecision displayPrecision' = case displayPrecision' of
  YMD.MonthPrecision -> CI.Month
  _ -> CI.Day

lmap :: (a -> a') -> (a,b) -> (a',b)
lmap f (a,b) = (f(a),b)

rmap :: (b -> b') -> (a,b) -> (a,b')
rmap f (a,b) = (a,f(b))

formRowCol :: (Renderable a)
           => a -- ^ label of the label field
           -> [DOMElement] -- ^ other columns
           -> DOMElement
formRowCol formFieldLabel otherColumns =
  div' (class' "form-group") [ 
    (label' (class'' ["control-label", "col-md-3"]) formFieldLabel) : otherColumns]

formRow :: (Renderable a)
        => a -- ^ label of field
        -> DOMElement -- ^ the other field
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
  theCheckbox = I.input mkAttrs inputAttrs
  in div' (class' "checkbox") $ label theCheckbox

editingInput :: String -> (SyntheticEvent -> Fay ()) -> Bool -> Bool -> DOMElement
editingInput = editingInput' False

editingTextarea :: String -> (SyntheticEvent -> Fay ()) -> Bool -> Bool -> DOMElement
editingTextarea = editingInput' True

editingInput' :: Bool -> String -> (SyntheticEvent -> Fay ()) -> Bool -> Bool -> DOMElement
editingInput' textarea value' onChange' editing' intMode = let
  inputAttrs = let
    commonInputAttrs = if textarea
      then I.mkInputAttrs
      else I.mkInputAttrs {
        I.value_ = Defined $ if intMode && (pack value' == "0")
          then ""
          else pack value' }
    in if editing' 
      then commonInputAttrs {
        I.onChange = Defined onChange' }
      else commonInputAttrs { 
        I.disabled_ = Defined "disabled" }
  in if textarea 
    then I.textarea inputNormalAttrs inputAttrs (pack value')
    else I.input inputNormalAttrs inputAttrs

formRow' :: Text -> String -> (SyntheticEvent -> Fay ()) -> Bool -> Bool -> DOMElement
formRow' labelText value' onChange' editing' intMode = 
  formRow labelText $ editingInput value' onChange' editing' intMode

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

eventInt :: (Int -> Fay ()) -> SyntheticEvent -> Fay ()
eventInt fun = eventValue >=> (\text -> case parseSafely text of
  Just(int) -> fun int
  Nothing | text == "" -> fun 0
  Nothing -> return () )

inputNormalAttrs :: Attributes
inputNormalAttrs = class' "form-control"
