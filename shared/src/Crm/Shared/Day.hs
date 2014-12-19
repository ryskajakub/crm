{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Shared.Day (
  Day(..)
) where

#ifndef FAY
import "base" Prelude
#else
import "fay-base" Prelude
#endif

-- | year, month, day
newtype Day = Day { getDay :: (Integer, Int, Int) } 
