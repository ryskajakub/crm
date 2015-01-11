{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Shared.UpkeepSequence where

#ifndef FAY
import GHC.Generics
import "base" Data.Data
import "base" Prelude
#else
import "fay-base" Prelude
#endif

data UpkeepSequence = UpkeepSequence {
  displayOrdering :: Int , 
  label_ :: String ,
  repetition :: Int ,
  oneTime :: Bool }
#ifndef FAY
  deriving (Generic, Typeable, Data, Show)
#endif

newUpkeepSequence :: UpkeepSequence
newUpkeepSequence = UpkeepSequence 0 "" 0 False
