{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Page.Employee (
  employeePage ) where

import "fay-base" Data.Text (fromString) 
import "fay-base" Prelude hiding (div, span, id)

import HaskellReact

import qualified Crm.Shared.Employee as E

employeePage :: [(E.EmployeeId, E.Employee)] 
             -> DOMElement
employeePage _ =
  div "Employees"
