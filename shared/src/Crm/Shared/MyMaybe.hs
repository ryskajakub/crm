{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}

module Crm.Shared.MyMaybe where

#ifndef FAY
import GHC.Generics
import Data.Data
#endif

data MyMaybe a = MyJust a | MyNothing
#ifdef FAY
  deriving (Eq)
#else
  deriving (Generic, Typeable, Data, Show, Eq)
#endif

toMaybe :: MyMaybe a -> Maybe a
toMaybe MyNothing = Nothing
toMaybe (MyJust a) = Just a

toMyMaybe :: Maybe a -> MyMaybe a
toMyMaybe Nothing = MyNothing
toMyMaybe (Just a) = MyJust a
