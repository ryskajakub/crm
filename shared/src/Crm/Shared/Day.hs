{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Shared.Day (
  Day(..)
) where

#ifndef FAY
import GHC.Generics
import "base" Data.Data
import "base" Prelude
#else
import "fay-base" Prelude
#endif

-- | year, month, day
data Day = Day { 
  year :: Int ,
  month :: Int ,
  day :: Int }
  deriving (Generic, Typeable, Data, Show)
