{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Page.Other.MachineType (
  machineTypesList ,
  machineTypeForm ) where

import "fay-base" Data.Text (fromString, unpack, pack) 
import "fay-base" Prelude hiding (div, span, id)

import HaskellReact

machineTypeForm = span "Machine type form"

machineTypesList = span "Machine types list"
