{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Shared.PhotoMeta where

#ifndef FAY
import GHC.Generics
import Data.Data
#endif
import Data.Text (Text)

data PhotoMeta = PhotoMeta {
  mimeType :: Text ,
  fileName :: Text }
#ifndef FAY
  deriving (Generic, Typeable, Data)
#endif
