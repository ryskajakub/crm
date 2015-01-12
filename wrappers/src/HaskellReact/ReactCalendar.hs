{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HaskellReact.ReactCalendar (
  month
  , MonthProps(..)
) where

import "fay-base" FFI
import "fay-base" Data.Text (Text, pack)
import "fay-base" Prelude

import qualified HaskellReact as HR

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
  onClick :: Defined (Text -> MomentObject -> HR.SyntheticMouseEvent -> Fay ()) ,
  date :: MomentObject }

data DayProps = DayProps {
  onClick_ :: Defined (Text -> MomentObject -> HR.SyntheticMouseEvent -> Fay ()) }

month :: MomentObject
      -> (Int -> Int -> Int -> Text -> Fay ())
      -> HR.DOMElement
month momentObject setDate = let
  clickHandler text moment eventObject = do
    let (year', month', day') = day moment 
    HR.stopPropagation eventObject
    setDate year' month' day' text
  monthProps = MonthProps {
    onClick = Defined clickHandler ,
    date = momentObject }
  dayProps = DayProps {
    onClick_ = Defined clickHandler }
  in reactCalendar (pack "Month") monthProps (
    reactCalendar (pack "Day") dayProps ([]::[HR.DOMElement]) )
