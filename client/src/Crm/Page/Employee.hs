{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Page.Employee (
  employeePage ,
  newEmployeeForm ) where

import "fay-base" Data.Text (fromString, pack)
import "fay-base" Prelude hiding (div, span, id)
import "fay-base" FFI (Defined (Defined))
import "fay-base" Data.Var (Var, modify)

import HaskellReact
import qualified HaskellReact.Bootstrap as B
import qualified HaskellReact.Bootstrap.Button as BTN
import qualified HaskellReact.Bootstrap.Glyphicon as G

import Crm.Server (createEmployee)
import Crm.Component.Form (row', saveButtonRow')
import qualified Crm.Data.Data as D
import qualified Crm.Data.EmployeeData as ED
import qualified Crm.Shared.Employee as E
import Crm.Router (CrmRouter, navigate, newEmployee)
import qualified Crm.Router as R
import Crm.Helpers (pageInfo, validationHtml)

employeePage :: CrmRouter
             -> [(E.EmployeeId, E.Employee)] 
             -> DOMElement
employeePage router employees = let 
  rowEmployee (employeeId, employee) = tr [ 
    td $ R.link (pack $ E.name employee) (R.editEmployee employeeId) router ,
    td $ pack $ E.contact employee ,
    td $ pack $ E.capabilities employee ]
  goToAddEmployee = navigate newEmployee router
  addEmployeeButton = BTN.button'
    (BTN.buttonProps {
      BTN.onClick = Defined $ const goToAddEmployee })
    [G.plus, text2DOM " Přidat servismana"]
  head' =
    thead $ tr [
      th $ "Jméno" ,
      th $ "Kontakt" ,
      th $ "Kvalifikace" ]
  body = tbody $ map rowEmployee employees
  in B.grid [
    B.row $ B.col (B.mkColProps 12) $ h2 "Servismani" ,
    B.row $ B.col (B.mkColProps 12) $ addEmployeeButton ,
    B.row $ B.col (B.mkColProps 12) $ B.table [ head' , body ]]

newEmployeeForm :: CrmRouter
                -> E.Employee
                -> Var D.AppState
                -> DOMElement
newEmployeeForm = employeeForm pageInfo' where
  pageInfo' = pageInfo "Nový servisman" $ Just "Tady můžeš přídat nového servismana, pokud najmete nového zaměstnance, nebo pokud využijete služeb někoho externího."

employeeForm :: (Renderable a)
             => a
             -> CrmRouter
             -> E.Employee
             -> Var D.AppState
             -> DOMElement
employeeForm pageInfo' router employee appVar = let 

  modify' :: E.Employee -> Fay ()
  modify' employee' = modify appVar (\appState -> appState {
    D.navigation = case D.navigation appState of 
      D.EmployeeManage (ED.EmployeeData _ a) -> D.EmployeeManage (ED.EmployeeData employee' a)
      _ -> D.navigation appState })

  validationMessages = if (length $ E.name employee) > 0
    then []
    else ["Jméno musí mít alespoň jeden znak."]
  in form' (mkAttrs { className = Defined "form-horizontal" }) $ 
    (B.grid $ (B.row $ pageInfo') : [
      row'
        True 
        "Jméno" 
        (E.name employee) 
        (eventString >=> (\employeeName -> modify' $ employee { E.name = employeeName })) ,
      row'
        True
        "Kontakt"
        (E.contact employee)
        (eventString >=> (\employeeName -> modify' $ employee { E.contact = employeeName })) ,
      row'
        True
        "Kvalifikace"
        (E.capabilities employee)
        (eventString >=> (\employeeName -> modify' $ employee { E.capabilities = employeeName })) ,
      B.row $ B.col (B.mkColProps 12) $ div' (class' "form-group") $ saveButtonRow' (null validationMessages)
        "Přidat servismena"
        (createEmployee employee $ navigate R.employeePage router)]) :
    (validationHtml validationMessages) : []
