{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Crm.Page.ContactPerson (
  contactPersonsList ,
  contactPersonForm ) where

import Data.Text (fromString, length)
import Prelude hiding (div, length)
import Data.Var (Var, modify)
import FFI (Defined (Defined))

import HaskellReact as HR
import qualified HaskellReact.Bootstrap as B

import qualified Crm.Shared.Company as C
import qualified Crm.Shared.ContactPerson as CP

import Crm.Component.Form
import Crm.Helpers (pageInfo, validationHtml)
import Crm.Server (createContactPerson, updateContactPerson)
import qualified Crm.Data.Data as D
import qualified Crm.Router as R

contactPersonsList :: R.CrmRouter
                   -> [(CP.ContactPersonId, CP.ContactPerson)]
                   -> DOMElement
contactPersonsList router contactPersons = let
  displayContactPerson (cpId,contactPerson) = tr [
    td $ R.link (CP.name contactPerson) (R.contactPersonEdit cpId) router ,
    td $ CP.phone contactPerson ,
    td $ CP.position contactPerson ]
  head' = thead $ tr [
    th $ "Jméno" ,
    th $ "Telefon" ,
    th $ "Pozice" ]
  body = tbody $ map displayContactPerson contactPersons
  in B.grid $ B.row [
    B.col (B.mkColProps 12) $ h2 "Kontakní osoby" ,
    B.col (B.mkColProps 12) $ B.table [head', body] ]

contactPersonForm :: R.CrmRouter 
                  -> CP.ContactPerson
                  -> Maybe CP.ContactPersonId
                  -> C.CompanyId
                  -> Var D.AppState
                  -> DOMElement
contactPersonForm router contactPerson identification companyId appVar = let

  modify' :: CP.ContactPerson -> Fay ()
  modify' contactPerson' = modify appVar (\appState -> appState {
    D.navigation = case D.navigation appState of 
      D.ContactPersonPage _ identification' c -> D.ContactPersonPage contactPerson' identification' c
      _ -> D.navigation appState })

  validationMessages = if (length $ CP.name contactPerson) > 0
    then []
    else ["Jméno musí mít alespoň jeden znak."]
  
  (buttonLabel, header, buttonAction) = case identification of
    Nothing -> ("Vytvoř", "Nová kontaktní osoba", createContactPerson
      companyId
      contactPerson
      (R.navigate (R.contactPersonList companyId) router ))
    Just contactPersonId -> ("Edituj", "Kontaktní osoba", updateContactPerson
      contactPersonId
      contactPerson
      (R.navigate (R.contactPersonList companyId) router ))

  pageInfo' = pageInfo header (Nothing :: Maybe DOMElement)

  in form' (mkAttrs { className = Defined "form-horizontal" }) $ 
    (B.grid $ (B.row $ pageInfo') : [
      inputRow
        Editing
        "Jméno" 
        (SetValue $ CP.name contactPerson)
        (eventValue >=> (\t -> modify' $ contactPerson { CP.name = t })) ,
      inputRow
        Editing
        "Telefon"
        (SetValue $ CP.phone contactPerson)
        (eventValue >=> (\t -> modify' $ contactPerson { CP.phone = t })) ,
      inputRow
        Editing
        "Pozice"
        (SetValue $ CP.position contactPerson)
        (eventValue >=> (\t -> modify' $ contactPerson { CP.position = t })) ,
      B.row $ B.col (B.mkColProps 12) $ div' (class' "form-group") $ saveButtonRow'
        (buttonStateFromBool . null $ validationMessages)
        buttonLabel
        buttonAction]) :
    (validationHtml validationMessages) : []
