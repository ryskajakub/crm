{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Moment (
  Moment, MomentObject ,
  requireMoment, browserMoment ,
  now ,
  dayPrecision ,
  day ,
  parse ,
  diff ,
  addDays ,
  DiffType(..) ,
  format ) where

import FFI
import "fay-base" Prelude
import "fay-base" Data.Text (Text, pack)

-- | opaque data type representing moment library instance
data MomentObject

-- | opaque data type representing concrete moment in time
data Moment

-- | use CommonJS module
requireMoment :: Moment
requireMoment = ffi " (function(){var m = require('moment');m.locale('cs'); return m;})() "

-- | conjure up the moment from the thin air
browserMoment :: Moment
browserMoment = ffi " moment "

now :: Moment -> MomentObject
now = ffi " %1() "

addDays :: MomentObject -> Int -> MomentObject
addDays = ffi " (%1).clone().add(%2, 'days') "

dayPrecision :: Int -- ^ year
             -> Int -- ^ month
             -> Int -- ^ day
             -> Moment
             -> MomentObject
dayPrecision = ffi " %4().year(%1).month(%2).date(%3) "

format :: MomentObject -> Text -> Text
format = ffi " %1['format'](%2) "

parse' :: Moment -> Text -> MomentObject
parse' = ffi "%1(%2, \"D.M.YYYY\", true)"

isValid :: MomentObject -> Bool
isValid = ffi "%1['isValid']()"

parse :: Moment -> Text -> Maybe MomentObject
parse moment text = let 
  maybeDate = parse' moment text
  in if isValid maybeDate 
    then Just maybeDate
    else Nothing

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

data DiffType = Days | Years

diff' :: MomentObject -> MomentObject -> Text -> Int
diff' = ffi " %1.diff(%2, %3) "

diff :: MomentObject -> MomentObject -> DiffType -> Int
diff obj1 obj2 diffType = diff' obj1 obj2 diffTypeString where
  diffTypeString = pack $
    case diffType of
      Days -> "days"
      Years -> "years"
