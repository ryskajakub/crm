{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HaskellReact.Bootstrap.CalendarInput (
  dayInput
) where

import HaskellReact
import HaskellReact.Bootstrap.Popover as P
import "fay-base" Data.Text (fromString, Text)
import "fay-base" Prelude
import qualified Moment as M
import HaskellReact.ReactCalendar as RC
import qualified HaskellReact.Tag.Input as I
import FFI (Defined(Defined))

monthCalendar :: M.MomentObject -> DOMElement
monthCalendar moment = RC.month (RC.MonthProps {RC.date = moment})

-- | display the input with calendar set to the date consisting of y m d from the parameters
dayInput :: Int -- ^ year
         -> Int -- ^ month
         -> Int -- ^ day
         -> (Int -> Int -> Int -> Fay ()) -- ^ action to execute on day pick
         -> Bool -- ^ is the day picker open
         -> (Bool -> Fay ()) -- ^ set the openness of the picker
         -> [DOMElement]
dayInput y m d onDayPick pickerOpen setPickerOpen = let
  attrs = mkAttrs {
    className = Defined "form-control" ,
    onClick = Defined $ const $ setPickerOpen $ not pickerOpen }
  inputAttrs = I.mkInputAttrs
  input = I.input attrs inputAttrs
  picker =
    if pickerOpen
    then [ P.popover (P.mkPopoverProps P.placementBottom 0 0) $ 
      div' (class'' ["nowrap", "relative"]) $
        let 
          momentFromParams = M.dayPrecision y m d M.requireMoment 
          in monthCalendar momentFromParams ]
    else []
  in input : picker 
