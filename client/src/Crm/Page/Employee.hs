{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Page.Employee (
  employeePage ) where

import "fay-base" Data.Text (fromString, pack)
import "fay-base" Prelude hiding (div, span, id)

import HaskellReact
import qualified HaskellReact.Bootstrap as B

import qualified Crm.Shared.Employee as E

employeePage :: [(E.EmployeeId, E.Employee)] 
             -> DOMElement
employeePage employees = let 
  listEmployee employee = li $ pack $ E.name $ snd employee
  in B.grid [
    B.row $ B.col (B.mkColProps 12) $ h2 "Servismani" ,
    B.row $ B.col (B.mkColProps 12) $ ul' (class' "list-unstyled") $ map listEmployee employees ]
