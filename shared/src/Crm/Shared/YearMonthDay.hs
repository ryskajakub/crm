{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Shared.YearMonthDay (
  YearMonthDay(..) ,
  Precision(..) ) where

#ifndef FAY
import GHC.Generics
import "base" Data.Data
import "base" Prelude
#else
import "fay-base" Prelude
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
