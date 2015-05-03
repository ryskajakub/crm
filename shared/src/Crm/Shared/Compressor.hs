{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Shared.Compressor where

#ifndef FAY
import GHC.Generics
import "base" Data.Data
import "base" Prelude
#else
import "fay-base" Prelude
#endif

data Compressor = Compressor {
  note :: String }
#ifndef FAY
  deriving (Generic, Typeable, Data, Show)
#endif

newCompressor :: Compressor
newCompressor = Compressor {
  note = "" }
