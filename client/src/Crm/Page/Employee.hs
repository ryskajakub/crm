{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Crm.Page.Employee (
  employeeEdit ,
  employeePage ,
  newEmployeeForm ) where

import           Data.Text                        (fromString, Text, length)
import           Prelude                          hiding (div, span, id, length)
import           FFI                              (Defined (Defined))
import           Data.Var                         (Var, modify)

import           HaskellReact
import qualified HaskellReact.Bootstrap           as B
import qualified HaskellReact.Bootstrap.Button    as BTN
import qualified HaskellReact.Bootstrap.Glyphicon as G

import           Crm.Server                       (createEmployee, updateEmployee)
import           Crm.Component.Form
import qualified Crm.Data.Data                    as D
import qualified Crm.Data.EmployeeData            as ED
import qualified Crm.Shared.Employee              as E
import           Crm.Router                       (CrmRouter, navigate, newEmployee)
import qualified Crm.Router                       as R
import           Crm.Helpers                      (pageInfo, validationHtml)


employeePage :: CrmRouter
             -> [(E.EmployeeId, E.Employee)]
             -> DOMElement
employeePage router employees = mkGrid where

  mkEmployeeRow (employeeId, employee) = tr [ 
    td $ R.link (E.name employee) (R.editEmployee employeeId) router ,
    td $ E.contact employee ,
    td $ E.capabilities employee ]

  addEmployeeButton = BTN.button'
    (BTN.buttonProps {
      BTN.onClick = Defined $ const goToAddEmployee })
    [G.plus, text2DOM " Přidat servismana"]
    where
    goToAddEmployee = navigate newEmployee router

  mkGrid = B.grid [
    B.row $ B.col (B.mkColProps 12) $ h2 "Servismani" ,
    B.row $ B.col (B.mkColProps 12) $ addEmployeeButton ,
    B.row $ B.col (B.mkColProps 12) $ B.table [ head' , body ]]
    where
    head' =
      thead $ tr [
        th $ "Jméno" ,
        th $ "Kontakt" ,
        th $ "Kvalifikace" ]
    body = tbody $ map mkEmployeeRow employees


newEmployeeForm :: CrmRouter
                -> E.Employee
                -> Var D.AppState
                -> DOMElement
newEmployeeForm router employee = employeeForm pageInfo' (buttonLabel, buttonAction) employee where
  buttonLabel = "Přidat servismena"
  buttonAction = createEmployee employee $ navigate R.employeePage router
  pageInfo' = pageInfo "Nový servisman" $ Just "Tady můžeš přídat nového servismana, pokud najmete nového zaměstnance, nebo pokud využijete služeb někoho externího."


employeeEdit :: E.EmployeeId
             -> CrmRouter
             -> E.Employee
             -> Var D.AppState
             -> DOMElement
employeeEdit employeeId router employee = employeeForm pageInfo' (buttonLabel, buttonAction) employee where
  buttonLabel = "Edituj"
  buttonAction = updateEmployee employeeId employee $ navigate R.employeePage router
  pageInfo' = pageInfo "Editace servismena" (Nothing :: Maybe DOMElement)

employeeForm :: (Renderable a)
             => a
             -> (Text, Fay ())
             -> E.Employee
             -> Var D.AppState
             -> DOMElement
employeeForm pageInfo' (buttonLabel, buttonAction) employee appVar = mkForm where

  modify' :: E.Employee -> Fay ()
  modify' employee' = modify appVar (\appState -> appState {
    D.navigation = case D.navigation appState of 
      D.EmployeeManage (ED.EmployeeData _ a) -> D.EmployeeManage (ED.EmployeeData employee' a)
      _ -> D.navigation appState })

  validationMessages = if (length $ E.name employee) > 0
    then []
    else ["Jméno musí mít alespoň jeden znak."]

  mkForm = form' (mkAttrs { className = Defined "form-horizontal" }) $ 
    (B.grid $ (B.row $ pageInfo') : [
      inputRowEditing
        "Jméno" 
        (SetValue $ E.name employee) $ 
        \employeeName -> modify' $ employee { E.name = employeeName } ,
      inputRowEditing
        "Kontakt"
        (SetValue $ E.contact employee) $ 
        \employeeName -> modify' $ employee { E.contact = employeeName } ,
      inputRowEditing
        "Kvalifikace"
        (SetValue $ E.capabilities employee) $ 
        \employeeName -> modify' $ employee { E.capabilities = employeeName } ,
      B.row $ B.col (B.mkColProps 12) $ div' (class' "form-group") $ buttonRow'
        (buttonStateFromBool . null $ validationMessages)
        buttonLabel
        buttonAction]) :
    (validationHtml validationMessages) : []
    where
    inputRowEditing = inputRow Editing
