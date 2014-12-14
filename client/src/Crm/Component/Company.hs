{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Component.Company (
  companiesList
  , companyDetail
) where

import HaskellReact as HR
import qualified Crm.Shared.Company as C
import qualified Crm.Shared.Machine as M
import "fay-base" Data.Text (fromString, unpack, pack, append, showInt)
import "fay-base" Prelude hiding (div, span, id)
import Data.Var (Var, modify)
import FFI (Defined(Defined))
import HaskellReact.BackboneRouter (link)
import qualified HaskellReact.Bootstrap as B
import qualified HaskellReact.Bootstrap.Glyphicon as G
import Crm.Component.Data
import Crm.Component.Editable (editable)

companiesList :: MyData
              -> [(Int, C.Company)]
              -> DOMElement
companiesList myData companies' = let
  head' =
    thead $ tr [
      th "Název firmy"
      , th "Platnost servisu vyprší za"
    ]
  body = tbody $ map (\idCompany ->
    let (id', company') = idCompany
    in tr [
      td $
        link
          (pack $ C.companyName company')
          ("/companies/" `append` (showInt id'))
          (router myData)
      , td $ pack $ C.companyPlant company'
    ]) companies'
  in main [
    section $
      B.button [
        G.plus
        , text2DOM "Přidat firmu"
      ]
    , section $
      B.table [
        head'
        , body
      ]
    ]

companyDetail :: Bool -- ^ is the page editing mode
              -> MyData -- ^ common read data
              -> Var (AppState) -- ^ app state var, where the editing result can be set
              -> (Int, C.Company) -- ^ company, which data are displayed on this screen
              -> [M.Machine] -- ^ machines of the company
              -> DOMElement -- ^ company detail page fraction
companyDetail editing' _ var idCompany machines' = let
  (id', company') = idCompany
  machineBox machine =
    B.col (B.ColProps 4) $
      B.panel [
        h2 $ span $ pack $ M.machineName machine
        , dl [
          dt "Další servis"
          , dd ""
          ]
      ]
  machineBoxes = map machineBox machines'
  in main [
    section $ let
      editButton = let
        editButtonBody = [G.pencil, HR.text2DOM " Editovat"]
        editButtonHandler _ = modify var (\appState ->
          appState {
            navigation = case navigation appState of
              cd @ (CompanyDetail _ _ _ _) -> cd { editing = True }
              _ -> navigation appState
          })
        editButtonProps = B.buttonProps {B.onClick = Defined editButtonHandler}
        in HR.reactInstance2DOM $ B.button' editButtonProps editButtonBody
      headerDisplay = h1 $ pack $ C.companyName company'
      headerSet newHeader = modify var (\appState -> appState {
          navigation = case navigation appState of
            cd @ (CompanyDetail _ _ _ _) -> cd { company = company' { C.companyName = unpack $ newHeader } }
            _ -> navigation appState
        })
      header = editable editing' headerDisplay (pack $ C.companyName company') headerSet
      saveHandler _ = modify var (\appState -> let
        companies' = companies appState
        (before, after) = break (\(cId, _) -> cId == id') companies'
        newCompanies = before ++ [idCompany] ++ tail after
        in appState { companies = newCompanies })

      saveEditButton' = HR.reactInstance2DOM $ B.button' (B.buttonProps {
        B.onClick = Defined saveHandler
        , B.bsStyle = Defined "primary"
        }) "Uložit"
      saveEditButton = if editing'
        then [saveEditButton']
        else []
      companyBasicInfo = [
        header
        , dl $ [
          dt "Adresa"
          , dd ""
          , dt "Kontakt"
          , dd ""
          , dt "Telefon"
          , dd ""
          ] ++ saveEditButton
        ]
      companyBasicInfo' = if editing' then companyBasicInfo else editButton:companyBasicInfo
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
