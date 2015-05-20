{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}

module Crm.Shared.ContactPerson where

#ifndef FAY
import GHC.Generics
import Data.Data
#endif
import Data.Text (Text, pack)

newtype ContactPersonId = ContactPersonId { getContactPersonId :: Int }
#ifdef FAY
  deriving Eq
#else
  deriving (Generic, Typeable, Data, Show)
#endif

type ContactPerson' = (ContactPersonId, ContactPerson)

data ContactPerson = ContactPerson {
  name :: Text , 
  phone :: Text ,
  position :: Text }
#ifndef FAY
  deriving (Generic, Typeable, Data)
#endif

newContactPerson :: ContactPerson
newContactPerson = ContactPerson (pack "") (pack "") (pack "")
