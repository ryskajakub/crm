{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PackageImports #-}

module Moment (
  Moment, MomentObject ,
  requireMoment, browserMoment ,
  now ,
  dayPrecision ,
  day ,
  Moment ) where

import FFI
import "fay-base" Prelude
import "fay-base" Data.Text (Text, pack)

-- | opaque data type representing moment library instance
data MomentObject

-- | opaque data type representing concrete moment in time
data Moment

-- | use CommonJS module
requireMoment :: Moment
requireMoment = ffi " require('moment') "

-- | conjure up the moment from the thin air
browserMoment :: Moment
browserMoment = ffi " moment "

now :: Moment -> MomentObject
now = ffi " %1() "

dayPrecision :: Int -- ^ year
             -> Int -- ^ month
             -> Int -- ^ day
             -> Moment
             -> MomentObject
dayPrecision = ffi " %4().year(%1).month(%2).date(%3) "

data GetDate 
  = Month
  | Day
  | Year

getFFI :: Text -> MomentObject -> Int
getFFI = ffi " %2[%1]() "

get :: GetDate -> MomentObject -> Int
get Month = getFFI (pack "month")
get Year = getFFI (pack "year")
get Day = getFFI (pack "date")

day :: MomentObject -> (Int, Int, Int)
day momentObject = let
  day' = get Day momentObject 
  month = get Month momentObject
  year = get Year momentObject
  in (year, month, day')
