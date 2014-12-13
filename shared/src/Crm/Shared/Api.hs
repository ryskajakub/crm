{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Shared.Api (
  companies, companiesClient
  , machines, machinesClient
) where

#ifndef FAY
import "base" Data.Char
import "base" Prelude
#else
import "fay-base" Prelude
import "fay-base" Data.Char
#endif

companies :: String
companies = "companies"

machines :: String
machines = "machines"

companiesClient :: String
companiesClient = firstToUpper companies

machinesClient :: String
machinesClient = firstToUpper machines

firstToUpper :: String -> String
firstToUpper str = case str of
  s:tr -> toUpper s : tr
  [] -> ""
