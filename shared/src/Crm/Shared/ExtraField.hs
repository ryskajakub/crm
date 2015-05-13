{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Shared.ExtraField where

#ifndef FAY
import GHC.Generics
import "base" Data.Data
import "base" Prelude
#else
import "fay-base" Prelude
#endif

newtype ExtraFieldId = ExtraFieldId { getExtraFieldId :: Int }
#ifndef FAY
  deriving (Generic, Typeable, Data, Show)
#endif

data ExtraFieldIdentification = 
  ToBeAssigned | 
  Assigned {
    extraFieldId :: ExtraFieldId }
#ifndef FAY
  deriving (Generic, Typeable, Data, Show)
#endif
