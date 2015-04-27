{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HaskellReact.Bootstrap.CalendarInput (
  dayInput ,
  ChangeView (..) ,
  DisplayDatePrecision (..) ) where

import "fay-base" FFI (Defined(Defined))
import "fay-base" Data.Text (fromString, Text, pack, unpack)
import "fay-base" Prelude hiding (span, id)

import HaskellReact as HR
import HaskellReact.Bootstrap.Popover as P
import HaskellReact.ReactCalendar as RC
import qualified HaskellReact.Tag.Input as I
import qualified HaskellReact.Tag.Hyperlink as A

import qualified Moment as M
import Moment

type PickDate = (Int -> Int -> Int -> Text -> Fay ())

monthCalendar :: M.MomentObject -> PickDate -> DOMElement
monthCalendar moment pickDate = RC.month 
  moment
  (\y m d t -> pickDate y m d t)

data ChangeView = PreviousYear | PreviousMonth | NextMonth | NextYear

data DisplayDatePrecision = Month | Day

-- | display the input with calendar set to the date consisting of y m d from the parameters
dayInput :: Bool -- ^ editing
         -> Either Text (Int, Int, Int, DisplayDatePrecision) -- ^ date displayed in the field or text
         -> (Int, Int) -- ^ year and month state of the calendar picker
         -> PickDate -- ^ action to execute on day pick
         -> (Text -> Fay ()) -- ^ callback to execute on user typing in the field
         -> Bool -- ^ is the day picker open
         -> (Bool -> Fay ()) -- ^ set the openness of the picker
         -> (ChangeView -> Fay ()) -- ^ callback used when the user click on next or previous year/month
         -> [DOMElement]
dayInput editing' displayedDate (pickerYear, pickerMonth)
    onDayPick userTyping pickerOpen setPickerOpen changeView = let
  attrs = mkAttrs {
    className = Defined "form-control" ,
    HR.onClick = Defined $ const $ setPickerOpen $ not pickerOpen }
  momentLibrary = requireMoment
  dateAsText = case displayedDate of
    Right (y, m, d, displayDatePrecision) -> let
      dateAsMoment = dayPrecision y m d momentLibrary
      in format dateAsMoment (case displayDatePrecision of 
        Month -> "MMMM YYYY"
        _ -> "LL")
    Left text -> text
  inputAttrs = I.mkInputAttrs {
    I.value_ = Defined $ dateAsText ,
    I.onChange = Defined $ userTyping <=< eventValue ,
    I.onBlur = Defined $ const $ case displayedDate of
      Right _ -> return () 
      Left text' -> let
        textWithoutSpaces = pack $ filter (\c -> c /= ' ') (unpack text')
        maybeDate = parse momentLibrary textWithoutSpaces
        in case maybeDate of
          Just theDate' -> let
            (y, m, d) = day theDate'
            in onDayPick y m d "Day"
          Nothing -> return () }
  input = I.input attrs inputAttrs
  overcast = div' 
    (mkAttrs { 
      HR.onClick = Defined $ const $ setPickerOpen False ,
      id = Defined "overcast" })
    ""
  picker =
    if pickerOpen
    then [overcast, P.popover (P.mkPopoverProps P.placementBottom 20 35) $ let
      anyDay = 1
      momentFromParams = M.dayPrecision pickerYear pickerMonth anyDay M.requireMoment 
      changeViewLink :: Text -> ChangeView -> Text -> DOMElement
      changeViewLink className' changeViewCommand content = let
        normalAttrs = (class' className') {
          HR.onClick = Defined $ const $ changeView changeViewCommand }
        in A.a'' normalAttrs A.mkAAttrs content
      in div' (class'' ["nowrap", "relative"]) [
        changeViewLink "previous-year" PreviousYear "<<" ,
        changeViewLink "previous-month" PreviousMonth "<" ,
        changeViewLink "next-month" NextMonth ">" ,
        changeViewLink "next-year" NextYear ">>" ,
        monthCalendar momentFromParams (\y' m' d' t' -> do 
          onDayPick y' m' d' t' 
          setPickerOpen False )]]
    else []
  display = 
    if editing'
    then input : picker
    else [text2DOM dateAsText]
  in display
