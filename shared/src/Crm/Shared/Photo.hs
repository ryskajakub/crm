{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}

module Crm.Shared.Photo where

#ifndef FAY
import Data.Data
import GHC.Generics
#endif

newtype PhotoId = PhotoId { getPhotoId :: Int }
#ifndef FAY
  deriving (Generic, Typeable, Data, Show)
#endif
