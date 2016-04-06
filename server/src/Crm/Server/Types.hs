{-# LANGUAGE KindSignatures #-}

module Crm.Server.Types where

import           Data.IORef                 (IORef)
import qualified Data.Map                   as M
import           Data.Pool                  (Pool)

import           Control.Monad.Reader       (ReaderT)
import           Database.PostgreSQL.Simple (Connection)

import qualified Crm.Shared.Company         as C
import qualified Crm.Shared.MachineType     as MT
import qualified Crm.Shared.MachineKind     as MK


type CacheContent = (C.Company, C.CompanyState, Maybe C.Coordinates, [MK.MachineKindEnum])

data MachineTypeMid = Autocomplete String | AutocompleteManufacturer String | CountListing
data MachineTypeSid = MachineTypeByName String | MachineTypeById MT.MachineTypeId

newtype Cache = Cache (IORef (M.Map C.CompanyId CacheContent))
type GlobalBindings = (Cache, ConnectionPool)

type Dependencies = (ReaderT GlobalBindings IO :: * -> *)
type IdDependencies = (ReaderT (GlobalBindings, Either String Int) IO :: * -> *)
type IdDependencies' a = (ReaderT (GlobalBindings, a) IO :: * -> *)
type StringIdDependencies = (ReaderT (GlobalBindings, String) IO :: * -> *)
type MachineTypeDependencies = (ReaderT (GlobalBindings, MachineTypeSid) IO :: * -> *)

type UrlId = Either String Int 

data Direction = Asc | Desc

type ConnectionPool = Pool Connection
