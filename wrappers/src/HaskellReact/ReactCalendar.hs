{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HaskellReact.ReactCalendar (
  month
  , MonthProps(..)
) where

import FFI
import qualified HaskellReact as HR
import "fay-base" Data.Text (Text, pack)
import "fay-base" Prelude
import Moment(MomentObject, day)

data ReactCalendar
instance HR.CommonJSModule ReactCalendar

requireReactCalendar :: ReactCalendar
requireReactCalendar = ffi " require('react-calendar') "
  
reactCalendar :: (HR.Renderable a)
              => Text -- ^ The name of the Bootstrap class
              -> b -- ^ The props passed to the instance
              -> a -- ^ The children passed to the instance
              -> HR.DOMElement
reactCalendar = HR.foreignReact requireReactCalendar

data MonthProps = MonthProps {
  date :: MomentObject }

data CalendarClickProps = CalendarClickProps {
  onClick :: Text -> MomentObject -> Fay () }

month :: MonthProps -> (Int -> Int -> Int -> Text -> Fay ()) -> HR.DOMElement
month monthProps setDate = reactCalendar (pack "Month") monthProps $ let
  calendarClickProps = (CalendarClickProps { onClick = \text moment -> let 
    (year, month, day') = day moment 
    in setDate year month day' text })
  in reactCalendar (pack "Day") calendarClickProps ([]::[HR.DOMElement])
