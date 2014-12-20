{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HaskellReact.ReactCalendar (
  month
  , MonthProps(..)
) where

import FFI
import HaskellReact
import "fay-base" Data.Text (Text, pack)
import "fay-base" Prelude
import Moment(MomentObject)

data ReactCalendar
instance CommonJSModule ReactCalendar

requireReactCalendar :: ReactCalendar
requireReactCalendar = ffi " require('react-calendar') "
  
reactCalendar :: (Renderable a)
              => Text -- ^ The name of the Bootstrap class
              -> b -- ^ The props passed to the instance
              -> a -- ^ The children passed to the instance
              -> DOMElement
reactCalendar = foreignReact requireReactCalendar

data MonthProps = MonthProps {
  date :: MomentObject }

month :: MonthProps -> DOMElement
month monthProps = reactCalendar (pack "Month") monthProps ([]::[DOMElement])
