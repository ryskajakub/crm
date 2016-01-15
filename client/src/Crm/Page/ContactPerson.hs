{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Crm.Page.ContactPerson (
  contactPersonsList ,
  contactPersonsList' ,
  contactPersonForm ) where

import           Data.Text                        (fromString, length)
import           Prelude                          hiding (div, length)
import           Data.Var                         (Var, modify)
import           FFI                              (Defined (Defined))

import           HaskellReact                     hiding (form)
import qualified HaskellReact.Bootstrap           as B
import qualified HaskellReact.Bootstrap.Button    as BTN
import qualified HaskellReact.Bootstrap.Glyphicon as G
import qualified HaskellReact.Bootstrap.Nav       as BN

import qualified Crm.Shared.Company               as C
import qualified Crm.Shared.ContactPerson         as CP

import           Crm.Component.Form
import           Crm.Helpers                      (pageInfo, validationHtml)
import           Crm.Server                       (createContactPerson, updateContactPerson, deleteContactPerson)
import qualified Crm.Data.Data                    as D
import qualified Crm.Router                       as R


contactPersonsList' :: 
  R.CrmRouter -> 
  [(CP.ContactPersonId, CP.ContactPerson)] -> 
  DOMElement
contactPersonsList' router contactPersons = mkTable where
  mkTable = B.row [
    B.col (B.mkColProps 12) $ h2 "Kontakní osoby" ,
    B.col (B.mkColProps 12) $ B.table [head', body]] where
      head' = thead $ tr [
        th $ "Jméno" ,
        th $ "Telefon" ,
        th $ "Pozice" ]
      body = tbody $ map displayContactPerson contactPersons
  displayContactPerson (cpId,contactPerson) = tr [
    td $ R.link (CP.name contactPerson) (R.contactPersonEdit cpId) router ,
    td $ CP.phone contactPerson ,
    td $ CP.position contactPerson ]


contactPersonsList :: 
  C.CompanyId ->
  R.CrmRouter -> 
  [(CP.ContactPersonId, CP.ContactPerson)] -> 
  DOMElement
contactPersonsList companyId router persons = B.grid [
  B.fullRow $ BN.nav [ R.link [G.arrowLeft, text2DOM " Zpět na firmu"] (R.companyDetail companyId) router ] ,
  contactPersonsList' router persons ]


contactPersonForm :: 
  R.CrmRouter -> 
  CP.ContactPerson -> 
  Maybe CP.ContactPersonId -> 
  C.CompanyId -> 
  Var D.AppState -> 
  DOMElement
contactPersonForm router contactPerson identification companyId appVar = mkForm where

  mkForm = form' (mkAttrs { className = Defined "form-horizontal" }) $ form : validations

  modify' :: CP.ContactPerson -> Fay ()
  modify' contactPerson' = modify appVar (\appState -> appState {
    D.navigation = case D.navigation appState of 
      D.ContactPersonPage _ identification' c -> D.ContactPersonPage contactPerson' identification' c
      _ -> D.navigation appState })

  validationMessages = if (length $ CP.name contactPerson) > 0
    then []
    else ["Jméno musí mít alespoň jeden znak."]
  
  (buttonLabel, header, buttonAction, deleteButton) = case identification of
    Nothing -> ("Vytvoř", "Nová kontaktní osoba", createContactPerson
      companyId
      contactPerson
      (R.navigate (R.contactPersonList companyId) router) 
      router, [])
    Just contactPersonId -> let
      deleteButton' = BTN.button' buttonProps "Smazat" where
        delete = deleteContactPerson contactPersonId (R.navigate (R.companyDetail companyId) router) router
        buttonProps = BTN.buttonProps {
          BTN.bsStyle = Defined "danger" ,
          BTN.onClick = Defined $ const delete }
      in ("Edituj", "Kontaktní osoba", updateContactPerson
        contactPersonId
        contactPerson
        (R.navigate (R.contactPersonList companyId) router)
        router, [deleteButton'])

  backToCompany = BN.nav [R.link [G.arrowLeft, text2DOM " Zpět na firmu"] (R.companyDetail companyId) router]
  form = div' (class' "contact-person") $ B.grid $ (B.fullRow backToCompany) : (B.row $ pageInfo') : [
    inputRow'
      "Jméno" 
      (SetValue $ CP.name contactPerson)
      (\t -> modify' $ contactPerson { CP.name = t }) ,
    inputRow'
      "Telefon"
      (SetValue $ CP.phone contactPerson)
      (\t -> modify' $ contactPerson { CP.phone = t }) ,
    inputRow'
      "Pozice"
      (SetValue $ CP.position contactPerson)
      (\t -> modify' $ contactPerson { CP.position = t }) ,
    editableRow Editing "" (saveButton : deleteButton)] where 
      saveButton = BTN.button' buttonProps buttonLabel where
        buttonProps = BTN.buttonProps {
          BTN.bsStyle = Defined "primary" ,
          BTN.onClick = Defined $ const buttonAction }
      pageInfo' = pageInfo header (Nothing :: Maybe DOMElement)
      inputRow' = inputRow Editing

  validations = [validationHtml validationMessages]
