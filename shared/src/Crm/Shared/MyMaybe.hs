{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Shared.MyMaybe where

#ifndef FAY
import GHC.Generics
import "base" Data.Data
import "base" Prelude
#else
import "fay-base" Prelude
#endif

data MyMaybe a = MyJust a | MyNothing
#ifndef FAY
  deriving (Generic, Typeable, Data, Show)
#endif

#ifdef FAY
instance Eq a => Eq (MyMaybe a)
#endif

toMaybe :: MyMaybe a -> Maybe a
toMaybe MyNothing = Nothing
toMaybe (MyJust a) = Just a

toMyMaybe :: Maybe a -> MyMaybe a
toMyMaybe Nothing = MyNothing
toMyMaybe (Just a) = MyJust a
