{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HaskellReact.Bootstrap.CalendarInput (
  dayInput
) where

import HaskellReact
import HaskellReact.Bootstrap.Popover as P
import "fay-base" Data.Text (fromString, Text, showInt, append)
import "fay-base" Prelude
import qualified Moment as M
import HaskellReact.ReactCalendar as RC
import qualified HaskellReact.Tag.Input as I
import "fay-base" FFI (Defined(Defined))

type PickDate = (Int -> Int -> Int -> Text -> Fay ())

monthCalendar :: M.MomentObject -> PickDate -> DOMElement
monthCalendar moment pickDate = RC.month (RC.MonthProps {RC.date = moment}) (\y m d t ->
  pickDate y (m + 1) d t)

-- | display the input with calendar set to the date consisting of y m d from the parameters
dayInput :: Int -- ^ year
         -> Int -- ^ month
         -> Int -- ^ day
         -> PickDate -- ^ action to execute on day pick
         -> Bool -- ^ is the day picker open
         -> (Bool -> Fay ()) -- ^ set the openness of the picker
         -> [DOMElement]
dayInput y m d onDayPick pickerOpen setPickerOpen = let
  attrs = mkAttrs {
    className = Defined "form-control" ,
    onClick = Defined $ const $ setPickerOpen $ not pickerOpen }
  inputAttrs = I.mkInputAttrs {
    I.value_ = Defined $ showInt d `append` "." `append` showInt m `append` "." `append` showInt y }
  input = I.input attrs inputAttrs
  picker =
    if pickerOpen
    then [ P.popover (P.mkPopoverProps P.placementBottom 0 0) $ 
      div' (class'' ["nowrap", "relative"]) $
        let 
          momentFromParams = M.dayPrecision y (m - 1) d M.requireMoment 
          in monthCalendar momentFromParams onDayPick ]
    else []
  in input : picker 
