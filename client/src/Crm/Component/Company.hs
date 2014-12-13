{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Component.Company (
  companiesList
  , companyDetail
) where

import HaskellReact
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


{-
        <Panel>
          <h2><a href="javascript://">{type}</a></h2>
          <dl>
            <dt>Další servis</dt>
            <dd>{lastMaintenance}</dd>
          </dl>
          <img src={imageSource} width="240" />
        </Panel>
-}


companyDetail :: MyData
              -> Company
              -> [M.Machine]
              -> DOMElement
companyDetail myData company machines = let
  machineBoxes = [B.col (B.ColProps 4) $ B.panel $ h2 "LLL aaa"]
  in main [
    section $
      B.jumbotron [
        h1 $ pack $ companyName company
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
