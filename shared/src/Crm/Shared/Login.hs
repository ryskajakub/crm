{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}

module Crm.Shared.Login where

#ifndef FAY
import GHC.Generics
import Data.Data
import Prelude
#endif
import Data.Text (Text)

data Login = Login {
  password :: Text }
#ifndef FAY
  deriving (Generic, Typeable, Data)
#endif
