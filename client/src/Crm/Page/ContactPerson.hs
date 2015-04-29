{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Page.ContactPerson (
  contactPersonForm ) where

import "fay-base" Data.Text (fromString) 
import "fay-base" Prelude hiding (div)
import "fay-base" Data.Var (Var, modify)
import "fay-base" FFI (Defined (Defined))

import HaskellReact as HR
import qualified HaskellReact.Bootstrap as B

import qualified Crm.Shared.Company as C
import qualified Crm.Shared.ContactPerson as CP

import Crm.Component.Form (row', saveButtonRow')
import Crm.Helpers (pageInfo, validationHtml)
import Crm.Server (createContactPerson)
import qualified Crm.Data.Data as D

contactPersonForm :: CP.ContactPerson
                  -> C.CompanyId
                  -> Var D.AppState
                  -> DOMElement
contactPersonForm contactPerson companyId appVar = let

  modify' :: CP.ContactPerson -> Fay ()
  modify' contactPerson' = modify appVar (\appState -> appState {
    D.navigation = case D.navigation appState of 
      D.ContactPersonPage _ companyId' -> D.ContactPersonPage contactPerson' companyId'
      _ -> D.navigation appState })

  pageInfo' = pageInfo "Nová kontaktní osoba" (Nothing :: Maybe DOMElement)

  validationMessages = if (length $ CP.name contactPerson) > 0
    then []
    else ["Jméno musí mít alespoň jeden znak."]
  
  buttonLabel = "Vytvoř"
  buttonAction = createContactPerson
    companyId
    contactPerson
    (return ())

  in form' (mkAttrs { className = Defined "form-horizontal" }) $ 
    (B.grid $ (B.row $ pageInfo') : [
      row'
        True 
        "Jméno" 
        (CP.name contactPerson)
        (eventString >=> (\t -> modify' $ contactPerson { CP.name = t })) ,
      row'
        True
        "Telefon"
        (CP.phone contactPerson)
        (eventString >=> (\t -> modify' $ contactPerson { CP.phone = t })) ,
      row'
        True
        "Pozice"
        (CP.position contactPerson)
        (eventString >=> (\t -> modify' $ contactPerson { CP.position = t })) ,
      B.row $ B.col (B.mkColProps 12) $ div' (class' "form-group") $ saveButtonRow'
        (null validationMessages)
        buttonLabel
        buttonAction]) :
    (validationHtml validationMessages) : []
