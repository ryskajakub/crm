module Crm.Server.Helpers (
  ymdToDay ,
  dayToYmd ) where

import qualified Crm.Shared.YearMonthDay as YMD
import Data.Time.Calendar (fromGregorian, Day, toGregorian)

ymdToDay :: YMD.YearMonthDay -> Day
ymdToDay ymd = day where 
  YMD.YearMonthDay year month day' _  = ymd
  day = fromGregorian (toInteger year) month day'

dayToYmd :: Day -> YMD.YearMonthDay
dayToYmd day = ymd where
  (year, month, day') = toGregorian day
  ymd = YMD.YearMonthDay (fromIntegral year) month day' YMD.DayPrecision
