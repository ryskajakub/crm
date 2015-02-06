{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Shared.Api (
  companies, companiesClient ,
  machines, machinesClient ,
  upkeep, upkeepsClient ,
  photos ,
  photoMeta ,
  planned ,
  machineTypes ,
  single ,
  autocomplete ,
  byName, byId ,
  employees ) where

#ifndef FAY
import "base" Data.Char
import "base" Prelude
#else
import "fay-base" Prelude
import "fay-base" Data.Char
#endif

companies :: String
companies = "companies"

machineTypes :: String
machineTypes = "machine-types"

machines :: String
machines = "machines"

upkeep :: String
upkeep = "upkeeps"

employees :: String
employees = "employees"

photos :: String
photos = "photos"

photoMeta :: String
photoMeta = "photo-meta"

planned :: String
planned = "planned"

autocomplete :: String
autocomplete = "autocomplete"

byId :: String
byId = "by-id"

byName :: String
byName = "by-name"

single :: String
single = "single"

companiesClient :: String
companiesClient = firstToUpper companies

machinesClient :: String
machinesClient = firstToUpper machines

upkeepsClient :: String
upkeepsClient = firstToUpper upkeep

firstToUpper :: String -> String
firstToUpper str = case str of
  s:tr -> toUpper s : tr
  [] -> ""
