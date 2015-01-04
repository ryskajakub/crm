{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HaskellReact.Bootstrap.CalendarInput (
  dayInput ,
  ChangeView (..) ,
  DisplayDatePrecision (..) ) where

import "fay-base" FFI (Defined(Defined))
import "fay-base" Data.Text (fromString, Text, showInt, append)
import "fay-base" Prelude hiding (span)

import qualified Moment as M

import HaskellReact as HR
import HaskellReact.Bootstrap.Popover as P
import HaskellReact.ReactCalendar as RC
import qualified HaskellReact.Tag.Input as I
import qualified HaskellReact.Tag.Hyperlink as A

import Moment

type PickDate = (Int -> Int -> Int -> Text -> Fay ())

monthCalendar :: M.MomentObject -> PickDate -> DOMElement
monthCalendar moment pickDate = RC.month 
  moment
  (\y m d t -> pickDate y (m + 1) d t)

data ChangeView = PreviousYear | PreviousMonth | NextMonth | NextYear

data DisplayDatePrecision = Month | Day

-- | display the input with calendar set to the date consisting of y m d from the parameters
dayInput :: Bool -- ^ editing
         -> Int -- ^ year
         -> Int -- ^ month
         -> Int -- ^ day
         -> DisplayDatePrecision
         -> PickDate -- ^ action to execute on day pick
         -> Bool -- ^ is the day picker open
         -> (Bool -> Fay ()) -- ^ set the openness of the picker
         -> (ChangeView -> Fay ()) -- ^ callback used when the user click on next or previous year/month
         -> [DOMElement]
dayInput editing' y m d displayDatePrecision onDayPick pickerOpen setPickerOpen changeView = let
  attrs = mkAttrs {
    className = Defined "form-control" ,
    HR.onClick = Defined $ const $ setPickerOpen $ not pickerOpen }
  momentLibrary = requireMoment
  dateAsMoment = dayPrecision y (m-1) d momentLibrary
  dateAsText = format dateAsMoment (case displayDatePrecision of 
    Day   -> "LL"
    Month -> "MMMM YYYY" )
  inputAttrs = I.mkInputAttrs {
    I.value_ = Defined dateAsText }
  input = I.input attrs inputAttrs
  picker =
    if pickerOpen
    then [ P.popover (P.mkPopoverProps P.placementBottom 20 35) $ let
      momentFromParams = M.dayPrecision y (m - 1) d M.requireMoment 
      changeViewLink :: Text -> ChangeView -> Text -> DOMElement
      changeViewLink className changeViewCommand content = let
        normalAttrs = (class' className) {
          HR.onClick = Defined $ const $ changeView changeViewCommand }
        in A.a'' normalAttrs A.mkAAttrs content
      in div' (class'' ["nowrap", "relative"]) [
        changeViewLink "previous-year" PreviousYear "<<" ,
        changeViewLink "previous-month" PreviousMonth "<" ,
        changeViewLink "next-month" NextMonth ">" ,
        changeViewLink "next-year" NextYear ">>" ,
        monthCalendar momentFromParams (\y m d t -> do 
          onDayPick y m d t 
          setPickerOpen False )]]
    else []
  display = 
    if editing'
    then input : picker
    else [text2DOM dateAsText]
  in display
