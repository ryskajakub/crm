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
instance Read PhotoId where 
  readsPrec i = fmap (\(a,b) -> (PhotoId a, b)) `fmap` readsPrec i
#endif

newtype PhotoId = PhotoId { getPhotoId :: Int }
#ifndef FAY
  deriving (Generic, Typeable, Data, Show)
#endif
