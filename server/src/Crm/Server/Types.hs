{-# LANGUAGE KindSignatures #-}

module Crm.Server.Types where

import Control.Monad.Reader (ReaderT)
import Database.PostgreSQL.Simple (Connection)

data MachineTypeMid = Autocomplete String | CountListing
data MachineTypeSid = MachineTypeByName String | MachineTypeById (Either String Int)

type Dependencies = (ReaderT Connection IO :: * -> *)
type IdDependencies = (ReaderT (Connection, Either String Int) IO :: * -> *)
type StringIdDependencies = (ReaderT (Connection, String) IO :: * -> *)
type MachineTypeDependencies = (ReaderT (Connection, MachineTypeSid) IO :: * -> *)

type UrlId = Either String Int 

data Direction = Asc | Desc
