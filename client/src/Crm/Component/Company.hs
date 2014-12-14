{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Component.Company (
  companiesList
  , companyDetail
  , companyNew
) where

import HaskellReact as HR
import qualified Crm.Shared.Company as C
import qualified Crm.Shared.Machine as M
import "fay-base" Data.Text (fromString, unpack, pack, append, showInt, Text)
import "fay-base" Prelude hiding (div, span, id)
import Data.Var (Var, modify)
import FFI (Defined(Defined))
import HaskellReact.BackboneRouter (link, navigate)
import qualified HaskellReact.Bootstrap as B
import qualified HaskellReact.Bootstrap.Button as BTN
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
      let
        buttonProps = BTN.buttonProps {
          BTN.onClick = Defined $ const $ navigate "/companies/new" (router myData)
          }
        in BTN.button' buttonProps [
          G.plus
          , text2DOM "Přidat firmu"
        ]
    , section $
      B.table [
        head'
        , body
      ]
    ]

companyNew :: MyData
           -> Var AppState
           -> C.Company
           -> DOMElement
companyNew myData var company' = let
  editing' = True
  saveHandler = do
    let newId = 999 :: Int
    modify var (\appState -> let
      companies' = companies appState
      newCompanies = companies' ++ [(newId, company')]
      in appState { companies = newCompanies })
  machines' = []
  setCompany modifiedCompany = modify var (\appState -> appState {
      navigation = case navigation appState of
        cd @ (CompanyNew _) -> cd { company = modifiedCompany }
        _ -> navigation appState
    })
  in companyPage editing' myData var setCompany company' saveHandler machines'

companyDetail :: Bool -- ^ is the page editing mode
              -> MyData -- ^ common read data
              -> Var (AppState) -- ^ app state var, where the editing result can be set
              -> (Int, C.Company) -- ^ company, which data are displayed on this screen
              -> [M.Machine] -- ^ machines of the company
              -> DOMElement -- ^ company detail page fraction
companyDetail editing' myData var idCompany machines' = let
  (id', company') = idCompany
  saveHandler = do
    modify var (\appState -> let
      companies' = companies appState
      (before, after) = break (\(cId, _) -> cId == id') companies'
      newCompanies = before ++ [idCompany] ++ tail after
      in appState { companies = newCompanies })
    navigate "" (router myData)
  setCompany modifiedCompany = modify var (\appState -> appState {
      navigation = case navigation appState of
        cd @ (CompanyDetail _ _ _ _) -> cd { company = modifiedCompany }
        _ -> navigation appState
    })
  in companyPage editing' myData var setCompany company' saveHandler machines'

companyPage :: Bool -- ^ is the page editing mode
            -> MyData -- ^ common read data
            -> Var (AppState) -- ^ app state var, where the editing result can be set
            -> (C.Company -> Fay ()) -- ^ modify the edited company data
            -> C.Company -- ^ company, which data are displayed on this screen
            -> Fay () -- ^ handler called when the user hits save
            -> [M.Machine] -- ^ machines of the company
            -> DOMElement -- ^ company detail page fraction
companyPage editing' _ var setCompany company' saveHandler' machines' = let
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
        editButtonProps = BTN.buttonProps {BTN.onClick = Defined editButtonHandler}
        in HR.reactInstance2DOM $ BTN.button' editButtonProps editButtonBody
      headerDisplay = h1 $ pack $ C.companyName company'
      headerSet newHeader = let
        company'' = company' {
          C.companyName = unpack $ newHeader
        }
        in setCompany company''
      header = editable editing' headerDisplay (pack $ C.companyName company') headerSet
      saveHandler _ = saveHandler'
      saveEditButton' = HR.reactInstance2DOM $ BTN.button' (BTN.buttonProps {
        BTN.onClick = Defined saveHandler
        , BTN.bsStyle = Defined "primary"
        }) "Uložit"
      saveEditButton = if editing'
        then [saveEditButton']
        else []
      companyBasicInfo = [
        header
        , dl $ [
          dt "Označení"
          , dd $ let
            plantDisplay = text2DOM $ pack $ C.companyPlant company'
            setCompanyPlant companyPlant' = let
              modifiedCompany = company' {
                C.companyPlant = unpack companyPlant' }
              in setCompany modifiedCompany
            in editable editing' plantDisplay (pack $ C.companyPlant company') setCompanyPlant
          , dt "Adresa"
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
