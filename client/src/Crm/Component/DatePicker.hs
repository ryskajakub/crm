{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Component.DatePicker (
  datePicker ,
  datePicker' ,
  DatePicker ,
  DatePickerData (..) ) where

import           Prelude                              hiding (putStrLn)
import           Data.Text                            (fromString, Text, unpack, putStrLn)

import qualified HaskellReact.Bootstrap.CalendarInput as CI
import           HaskellReact

import qualified Crm.Shared.YearMonthDay              as YMD
import           Crm.Helpers                          (displayPrecision, displayDate)
import           Crm.Component.Form

import           Debug.Trace

type DatePicker = (YMD.YearMonthDay, Bool)

data DatePickerData = DatePickerData {
  calendarDate :: YMD.YearMonthDay ,
  open :: Bool ,
  rawText :: Text }

datePicker' ::
  InputState ->
  DatePickerData ->
  (DatePickerData -> Fay ()) ->
  YMD.YearMonthDay ->
  (YMD.YearMonthDay -> Fay ()) ->
  [DOMElement]
datePicker' inputState datePickerData setDatePickerData date setDate = let
  DatePickerData calendarDate open rawText = datePickerData
  setDatePickerDate ymd = setDatePickerData $ datePickerData { calendarDate = ymd }
  setOpen open' = setDatePickerData $ datePickerData { open = open' }
  displayed = trace (show datePickerData ++ unpack " " ++ show date) $ if (rawText ==) . displayDate $ date
    then Right date
    else Left rawText
  setDisplayed (Left text) = setDatePickerData $ datePickerData { rawText = text }
  setDisplayed (Right newDate) = do
    setDate newDate
    setDatePickerData $ datePickerData { rawText = displayDate newDate }
  in datePicker inputState (calendarDate, open) setDatePickerDate setOpen displayed setDisplayed

datePicker :: InputState -- ^ editing
           -> DatePicker
           -> (YMD.YearMonthDay -> Fay ()) -- ^ set date picker date
           -> (Bool -> Fay ()) -- ^ set date picker openness
           -> Either Text YMD.YearMonthDay -- ^ displayed date or some text, that the user entered
           -> (Either Text YMD.YearMonthDay -> Fay ()) -- ^ set date
           -> [DOMElement]
datePicker editing (pickerStateDate, pickerStateOpen) setDatePickerDate
    setDatePickerOpenness displayedDateOrText setDate = let
  displayedDateOrText' = case displayedDateOrText of
    Right displayedDate -> let
      YMD.YearMonthDay y m d displayPrecision' = displayedDate
      in Right (y,m,d,(displayPrecision displayPrecision'))
    Left text -> Left text
  dayPickHandler :: Int -> Int -> Int -> Text -> Fay ()
  dayPickHandler year month day precision = case precision of
    month' | month' == "Month" -> setDate' YMD.MonthPrecision
    day' | day' == "Day" -> setDate' YMD.DayPrecision
    _ -> return ()
    where 
      setDate' precision' = do
        let dateToSet = YMD.YearMonthDay year month day precision'
        setDatePickerDate dateToSet
        setDate $ Right dateToSet
  userTypingHandler :: Text -> Fay ()
  userTypingHandler text = setDate $ Left text
  YMD.YearMonthDay pickerYear pickerMonth _ _ = pickerStateDate
  changeViewHandler changeViewCommand = let
    (newYear, newMonth) = case changeViewCommand of
      CI.PreviousYear           -> (pickerYear - 1, pickerMonth)
      CI.PreviousMonth | pickerMonth == 0 -> (pickerYear - 1, 11)
      CI.PreviousMonth          -> (pickerYear, pickerMonth - 1)
      CI.NextMonth | pickerMonth == 11    -> (pickerYear + 1, 0)
      CI.NextMonth              -> (pickerYear, pickerMonth + 1)
      CI.NextYear               -> (pickerYear + 1, pickerMonth)
    anyDay = 1
    newDate = YMD.YearMonthDay newYear newMonth anyDay YMD.DayPrecision
    in setDatePickerDate newDate
  in (CI.dayInput (inputStateToBool editing) displayedDateOrText' (pickerYear, pickerMonth) dayPickHandler 
    userTypingHandler (pickerStateOpen) setDatePickerOpenness changeViewHandler)
