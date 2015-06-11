{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}

module Crm.Shared.Photo where

#ifndef FAY
import Data.Data
import GHC.Generics
import Rest.Info    (Info(..))
#endif

#ifndef FAY
instance Info PhotoId where
  describe _ = "photoId"
#endif

newtype PhotoId = PhotoId { getPhotoId :: Int }
#ifndef FAY
  deriving (Generic, Typeable, Data, Show, Read)
#endif
