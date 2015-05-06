{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}

module Crm.Shared.Photo where

#ifndef FAY
import Data.Data
import GHC.Generics
import Data.JSON.Schema.Types (JSONSchema)
import Data.Aeson.Types (ToJSON, FromJSON)
#endif

newtype PhotoId = PhotoId { getPhotoId :: Int }
#ifndef FAY
  deriving (Generic, Typeable, Data, Show, JSONSchema, ToJSON, FromJSON)
#endif
