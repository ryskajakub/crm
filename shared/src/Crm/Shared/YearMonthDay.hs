{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Crm.Shared.YearMonthDay (
  YearMonthDay(..) ,
  Precision(..) ,
#ifndef FAY
  ymdToDay, dayToYmd ,
#endif
  new ) where

#ifndef FAY
import GHC.Generics
import Data.Data
import Opaleye            (QueryRunnerColumnDefault(..), queryRunnerColumn, 
                            fieldQueryRunnerColumn, PGDate)
import Data.Time.Calendar (fromGregorian, Day, toGregorian)
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

#ifndef FAY
instance QueryRunnerColumnDefault PGDate YearMonthDay where
  queryRunnerColumnDefault = 
    queryRunnerColumn id dayToYmd fieldQueryRunnerColumn

ymdToDay :: YearMonthDay -> Day
ymdToDay ymd = converted where 
  YearMonthDay year' month' day' _  = ymd
  converted = fromGregorian (toInteger year') (month' + 1) day'

dayToYmd :: Day -> YearMonthDay
dayToYmd day'' = ymd where
  (year', month', day') = toGregorian day''
  ymd = YearMonthDay (fromIntegral year') (month' - 1) day' DayPrecision

instance Eq YearMonthDay where
  YearMonthDay y m d _ == YearMonthDay y' m' d' _ = y == y' && m == m' && d == d'
instance Ord YearMonthDay where
  ymd1 `compare` ymd2 = let
    YearMonthDay y m d _ = ymd1
    YearMonthDay y' m' d' _ = ymd2
    comp comparison nextComparison = case comparison of
      GT -> GT
      LT -> LT
      EQ -> nextComparison
    in comp (y `compare` y') $ comp (m `compare` m') $ comp (d `compare` d') EQ
#endif
