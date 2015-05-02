{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Crm.Shared.ContactPerson where

#ifndef FAY
import GHC.Generics
import "base" Data.Data
import "base" Prelude
import Data.JSON.Schema.Types (JSONSchema)
import Data.Aeson.Types (ToJSON, FromJSON)
#else
import "fay-base" Prelude
#endif

newtype ContactPersonId = ContactPersonId { getContactPersonId :: Int }
#ifdef FAY
  deriving Eq
#else
  deriving (Data, Typeable, ToJSON, FromJSON, JSONSchema)
#endif

type ContactPerson' = (ContactPersonId, ContactPerson)

data ContactPerson = ContactPerson {
  name :: String , 
  phone :: String ,
  position :: String }
#ifndef FAY
  deriving (Generic, Typeable, Data, Show)
#endif

newContactPerson :: ContactPerson
newContactPerson = ContactPerson "" "" ""
