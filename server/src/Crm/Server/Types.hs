{-# LANGUAGE KindSignatures #-}

module Crm.Server.Types where

import Data.IORef                 (IORef)

import Control.Monad.Reader       (ReaderT)
import Database.PostgreSQL.Simple (Connection)

data MachineTypeMid = Autocomplete String | AutocompleteManufacturer String | CountListing
data MachineTypeSid = MachineTypeByName String | MachineTypeById (Either String Int)

type Cache = IORef Int
type GlobalBindings = (Cache, Connection)
type Dependencies = (ReaderT GlobalBindings IO :: * -> *)
type IdDependencies = (ReaderT (GlobalBindings, Either String Int) IO :: * -> *)
type StringIdDependencies = (ReaderT (GlobalBindings, String) IO :: * -> *)
type MachineTypeDependencies = (ReaderT (GlobalBindings, MachineTypeSid) IO :: * -> *)

type UrlId = Either String Int 

data Direction = Asc | Desc
