{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}

module Crm.Shared.ContactPerson where

#ifndef FAY
import GHC.Generics
import Data.Data
import Rest.Info    (Info(..))
#endif
import Data.Text    (Text, pack)

#ifndef FAY
instance Info ContactPersonId where
  describe _ = "contactPersonId"
instance Read ContactPersonId where 
  readsPrec i = fmap (\(a,b) -> (ContactPersonId a, b)) `fmap` readsPrec i
#endif

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
#ifdef FAY
  deriving Eq
#else
  deriving (Generic, Typeable, Data, Eq)
#endif

newContactPerson :: ContactPerson
newContactPerson = ContactPerson (pack "") (pack "") (pack "")
