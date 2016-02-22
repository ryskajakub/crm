{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}

module Crm.Shared.PhotoMeta where

#ifndef FAY
import GHC.Generics
import Data.Data
#endif
import Data.Text (Text)

data PhotoMeta = PhotoMeta {
  mimeType :: Text ,
  fileName :: Text ,
  source :: PhotoSource }
#ifndef FAY
  deriving (Generic, Typeable, Data)
#endif

data PhotoSource = IPhone | Other | Unknown
#ifndef FAY
  deriving (Generic, Typeable, Data, Show, Eq)
#endif
