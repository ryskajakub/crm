{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Shared.Date (
  Date(..)
) where

#ifndef FAY
import "base" Prelude
#else
import "fay-base" Prelude
#endif

-- | newtype over string represented in ISO 8601 format, that is yyyy-mm-dd
newtype Date = Date { getDate :: String }
