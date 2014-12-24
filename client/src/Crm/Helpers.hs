{-# LANGUAGE PackageImports #-}

module Crm.Helpers where

import "fay-base" Data.Text (Text, (<>), showInt, pack)
import FFI (Nullable, ffi)
import Data.Nullable (fromNullable)

import qualified Crm.Shared.YearMonthDay as YMD

parseInt :: Text -> Nullable Int
parseInt = ffi " (function() { var int = parseInt(%1); ret = ((typeof int) === 'number' && !isNaN(int)) ? int : null; return ret; })() "

parseSafely :: Text -> Maybe Int
parseSafely possibleNumber = fromNullable $ parseInt possibleNumber

displayDate :: YMD.YearMonthDay -> Text
displayDate (YMD.YearMonthDay y m d _) =
  showInt d <> pack "." <> showInt m <> pack "." <> showInt y
