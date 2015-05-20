{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}

module Crm.Shared.ExtraField where

#ifndef FAY
import GHC.Generics
import Data.Data
#endif

newtype ExtraFieldId = ExtraFieldId { getExtraFieldId :: Int }
#ifdef FAY
  deriving Eq
#else
  deriving (Generic, Typeable, Data, Show)
#endif

data ExtraFieldIdentification = 
  ToBeAssigned | 
  Assigned {
    extraFieldId :: ExtraFieldId }
#ifndef FAY
  deriving (Generic, Typeable, Data, Show)
#endif
