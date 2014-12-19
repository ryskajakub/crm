{-# LANGUAGE PackageImports #-}

module Crm.Helpers where

import "fay-base" Data.Text (Text)
import FFI (Nullable, ffi)
import Data.Nullable (fromNullable)

parseInt :: Text -> Nullable Int
parseInt = ffi " (function() { var int = parseInt(%1); ret = ((typeof int) === 'number' && !isNaN(int)) ? int : null; return ret; })() "

parseSafely :: Text -> Maybe Int
parseSafely possibleNumber = fromNullable $ parseInt possibleNumber
