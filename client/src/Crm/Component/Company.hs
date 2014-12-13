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
import Data.Var (Var, subscribeAndRead)
import "fay-base" Data.Maybe (fromMaybe, whenJust, fromJust)
import Data.Defined (fromDefined)
import FFI (Defined(Defined, Undefined))
import HaskellReact.BackboneRouter (BackboneRouter, link)
import qualified HaskellReact.Bootstrap as B
import qualified HaskellReact.Bootstrap.Glyphicon as G
import Crm.Component.Data

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
    section $
      B.jumbotron [
        let
          buttonBody = [G.pencil, HR.text2DOM " Editovat"]
          buttonProps = B.buttonProps {B.onClick = Defined $ const $ return ()}
          button = B.button' buttonProps buttonBody
          in HR.reactInstance2DOM button
        , h1 $ pack $ companyName company
        , dl [
          dt "Adresa"
          , dd ""
          , dt "Kontakt"
          , dd ""
          , dt "Telefon"
          , dd ""
        ]
      ]
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
