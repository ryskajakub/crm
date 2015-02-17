{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Page.Employee (
  employeePage ,
  employeeForm ) where

import "fay-base" Data.Text (fromString, pack)
import "fay-base" Prelude hiding (div, span, id)
import "fay-base" FFI (Defined (Defined))
import "fay-base" Data.Var (Var, modify)

import HaskellReact
import qualified HaskellReact.Bootstrap as B
import qualified HaskellReact.Bootstrap.Button as BTN
import qualified HaskellReact.Bootstrap.Glyphicon as G

import qualified Crm.Data.Data as D
import qualified Crm.Shared.Employee as E
import Crm.Router (CrmRouter, link, companyDetail, closeUpkeep, navigate, maintenances, 
  newEmployee)

employeePage :: CrmRouter
             -> [(E.EmployeeId, E.Employee)] 
             -> DOMElement
employeePage router employees = let 
  listEmployee employee = li $ pack $ E.name $ snd employee
  goToAddEmployee = navigate newEmployee router
  addEmployeeButton = BTN.button'
    (BTN.buttonProps {
      BTN.onClick = Defined $ const goToAddEmployee })
    [G.plus, text2DOM " Přidat servismana"]
  in B.grid [
    B.row $ B.col (B.mkColProps 12) $ h2 "Servismani" ,
    B.row $ B.col (B.mkColProps 12) $ addEmployeeButton ,
    B.row $ B.col (B.mkColProps 12) $ ul' (class' "list-unstyled") $ map listEmployee employees ]

employeeForm :: CrmRouter
             -> E.Employee
             -> Var D.AppState
             -> DOMElement
employeeForm _ _ _ = div "employee form"
