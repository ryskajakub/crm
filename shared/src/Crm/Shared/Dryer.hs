{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Shared.Dryer where

#ifndef FAY
import GHC.Generics
import "base" Data.Data
import "base" Prelude
#else
import "fay-base" Prelude
#endif

data Dryer = Dryer {
  note :: String }
#ifndef FAY
  deriving (Generic, Typeable, Data, Show)
#endif

newDryer :: Dryer
newDryer = Dryer ""
