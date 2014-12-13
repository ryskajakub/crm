{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Component.Company (
  companiesList
  , companyDetail
) where

import HaskellReact as HR
import Crm.Component.Navigation (navigation)
import Crm.Shared.Company
import qualified Crm.Shared.Machine as M
import "fay-base" Data.Text (fromString, Text, unpack, pack, append, showInt)
import "fay-base" Prelude hiding (div, span, id)
import Data.Var (Var, subscribeAndRead, set, modify)
import "fay-base" Data.Maybe (fromMaybe, whenJust, fromJust, onJust)
import Data.Defined (fromDefined)
import FFI (Defined(Defined, Undefined))
import HaskellReact.BackboneRouter (BackboneRouter, link)
import qualified HaskellReact.Bootstrap as B
import qualified HaskellReact.Bootstrap.Glyphicon as G
import qualified HaskellReact.Bootstrap.Input as I
import Crm.Component.Data
import Crm.Component.Editable (editable)

companiesList :: MyData
              -> [Company]
              -> DOMElement
companiesList myData companies = let
  head =
    thead $ tr [
      th "Název firmy"
      , th "Platnost servisu vyprší za"
    ]
  body = map (\company ->
    tr [
      td $
        link
          (pack $ companyName company)
          ("/companies/" `append` (showInt $ companyId company))
          (router myData)
      , td $ pack $ companyPlant company
    ]) companies
  in main [
    section $
      B.button [
        G.plus
        , text2DOM "Přidat firmu"
      ]
    , section $
      B.table [
        head : body
      ]
    ]

companyDetail :: Bool -- ^ is the page editing mode
              -> MyData -- ^ common read data
              -> Var (Maybe Company) -- ^ variable, that can will be used to store the edited company
              -> Company -- ^ company, which data are displayed on this screen
              -> [M.Machine] -- ^ machines of the company
              -> DOMElement -- ^ company detail page fraction
companyDetail editing myData var company machines = let
  machineBox machine =
    B.col (B.ColProps 4) $
      B.panel [
        h2 $ span $ pack $ M.machineName machine
        , dl [
          dt "Další servis"
          , dd ""
          ]
      ]
  machineBoxes = map machineBox machines
  in main [
    section $ let
      buttonBody = [G.pencil, HR.text2DOM " Editovat"]
      buttonHandler _ = set var $ Just company
      buttonProps = B.buttonProps {B.onClick = Defined buttonHandler}
      button = HR.reactInstance2DOM $ B.button' buttonProps buttonBody
      headerDisplay = h1 $ pack $ companyName company
      headerSet newHeader = modify var (\c -> onJust (\c' -> c' {companyName = unpack newHeader}) c)
      header = editable editing headerDisplay (pack $ companyName company) headerSet
      companyBasicInfo = [
        header
        , dl [
          dt "Adresa"
          , dd ""
          , dt "Kontakt"
          , dd ""
          , dt "Telefon"
          , dd ""
          ]
        ]
      companyBasicInfo' = if editing then companyBasicInfo else button:companyBasicInfo
      in B.jumbotron companyBasicInfo'
    , section $ B.grid [
      B.row $
        B.col (B.ColProps 12) $
          B.panel $
            span "Historie servisů"
      , B.row (machineBoxes ++ [
        B.col (B.ColProps 4) $ B.panel $ h2 "Nový stroj"
      ])
      , B.row $
        B.col (B.ColProps 12) $
          B.panel $
            span "Naplánovat servis"
    ]
  ]
