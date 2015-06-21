{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}

module Crm.Shared.YearMonthDay (
  YearMonthDay(..) ,
  Precision(..) ,
  new ) where

#ifndef FAY
import GHC.Generics
import Data.Data
#endif

-- | year, month, day
data YearMonthDay = YearMonthDay { 
  year :: Int , 
  month :: Int , -- ^ 0..11
  day :: Int , -- ^ 1..31
  precision :: Precision }
#ifndef FAY
  deriving (Generic, Typeable, Data, Show)
#endif

data Precision = DayPrecision | MonthPrecision
#ifndef FAY
  deriving (Generic, Typeable, Data, Show)
#endif

new :: YearMonthDay
new = YearMonthDay 1970 0 1 (DayPrecision)
