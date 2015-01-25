{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP #-}

module Crm.Shared.Direction where

#ifndef FAY
import "base" Prelude
#else
import "fay-base" Prelude
#endif

data Direction = Asc | Desc 
  deriving (Read, Show)
