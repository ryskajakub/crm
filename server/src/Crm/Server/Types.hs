{-# LANGUAGE KindSignatures #-}

module Crm.Server.Types where

import           Data.IORef                 (IORef)

import           Control.Monad.Reader       (ReaderT)
import           Database.PostgreSQL.Simple (Connection)

import qualified Crm.Shared.Company         as C
import qualified Crm.Shared.YearMonthDay    as YMD
import           Crm.Shared.MyMaybe

data MachineTypeMid = Autocomplete String | AutocompleteManufacturer String | CountListing
data MachineTypeSid = MachineTypeByName String | MachineTypeById (Either String Int)

type CoreData = [(C.CompanyId, C.Company, MyMaybe YMD.YearMonthDay, MyMaybe C.Coordinates)]
type Cache = IORef CoreData
type GlobalBindings = (Cache, Connection)

type Dependencies = (ReaderT GlobalBindings IO :: * -> *)
type IdDependencies = (ReaderT (GlobalBindings, Either String Int) IO :: * -> *)
type StringIdDependencies = (ReaderT (GlobalBindings, String) IO :: * -> *)
type MachineTypeDependencies = (ReaderT (GlobalBindings, MachineTypeSid) IO :: * -> *)

type UrlId = Either String Int 

data Direction = Asc | Desc
