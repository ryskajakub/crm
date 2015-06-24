{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}

module Crm.Shared.ServerRender where

#ifndef FAY
import Data.Data
import GHC.Generics
#endif
import Data.Text    (Text)

data Markup = 
  PlainText { getPlainText :: Text } |
  UnorderedList { getUnorderedList :: [Text] }
#ifndef FAY
  deriving (Generic, Typeable, Data, Show, Eq)
#endif
