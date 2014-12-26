{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Page.Company (
  companiesList , 
  companyDetail , 
  companyNew ) where

import "fay-base" Data.Text (fromString, unpack, pack) 
import "fay-base" Prelude hiding (div, span, id)
import Data.Var (Var, modify)
import FFI (Defined(Defined))
import Debug.Trace

import HaskellReact as HR
import qualified HaskellReact.Bootstrap as B
import qualified HaskellReact.Bootstrap.Button as BTN
import qualified HaskellReact.Bootstrap.Glyphicon as G

import qualified Crm.Shared.Company as C
import qualified Crm.Shared.Machine as M
import qualified Crm.Shared.MachineType as MT
import qualified Crm.Shared.YearMonthDay as YMD
import qualified Crm.Data as D
import Crm.Component.Editable (editable)
import Crm.Server (createCompany)
import qualified Crm.Router as R
import Crm.Helpers (displayDate)

companiesList :: R.CrmRouter
              -> [(Int, C.Company, Maybe YMD.YearMonthDay)]
              -> DOMElement
companiesList router companies' = let
  head' =
    thead $ tr [
      th "Název firmy" , 
      th "Platnost servisu vyprší za" , 
      th "Další servis" ]
  body = tbody $ map (\idCompany ->
    let (id', company', nextServiceDate) = idCompany
    in tr [
      td $
        R.link
          (pack $ C.companyName company')
          (R.companyDetail id')
          router ,
      td $ pack $ C.companyPlant company' , 
      td $ maybe "" displayDate nextServiceDate
    ]) companies'
  in trace (show companies') $ main [
    section $
      let
        buttonProps = BTN.buttonProps {
          BTN.onClick = Defined $ const $ R.navigate R.frontPage router }
        in BTN.button' buttonProps [
          G.plus , 
          text2DOM "Přidat firmu" ] ,
    section $
      B.table [
        head' , 
        body ] ]

companyNew :: R.CrmRouter
           -> Var D.AppState
           -> C.Company
           -> DOMElement
companyNew router var company' = let
  editing' = True
  saveHandler =
    createCompany company' (const $
      R.navigate R.frontPage router )
  machines' = []
  setCompany modifiedCompany = modify var (\appState -> appState {
    D.navigation = case D.navigation appState of
      cd @ (D.CompanyNew _) -> cd { D.company = modifiedCompany }
      _ -> D.navigation appState })
  in companyPage editing' router var setCompany company' (-666) saveHandler machines'

companyDetail :: Bool -- ^ is the page editing mode
              -> R.CrmRouter -- ^ common read data
              -> Var D.AppState -- ^ app state var, where the editing result can be set
              -> (Int, C.Company) -- ^ company, which data are displayed on this screen
              -> [(Int, M.Machine, Int, MT.MachineType)] -- ^ machines of the company
              -> DOMElement -- ^ company detail page fraction
companyDetail editing' router var idCompany machines' = let
  (id', company') = idCompany
  saveHandler =
    R.navigate R.frontPage router
  setCompany modifiedCompany = modify var (\appState -> appState {
    D.navigation = case D.navigation appState of
      cd @ (D.CompanyDetail _ _ _ _) -> cd { D.company = modifiedCompany }
      _ -> D.navigation appState })
  in companyPage editing' router var setCompany company' id' saveHandler machines'

companyPage :: Bool -- ^ is the page editing mode
            -> R.CrmRouter -- ^ common read data
            -> Var D.AppState -- ^ app state var, where the editing result can be set
            -> (C.Company -> Fay ()) -- ^ modify the edited company data
            -> C.Company -- ^ company, which data are displayed on this screen
            -> Int -- ^ company id
            -> Fay () -- ^ handler called when the user hits save
            -> [(Int, M.Machine, Int, MT.MachineType)] -- ^ machines of the company
            -> DOMElement -- ^ company detail page fraction
companyPage editing' router var setCompany company' companyId saveHandler' machines' = let
  machineBox (machineId', machine', _, machineType) =
    B.col (B.mkColProps 4) $
      B.panel [
        h2 $ 
          R.link
            (pack $ MT.machineTypeName machineType)
            (R.machineDetail machineId')
            router , 
        dl [
          dt "Další servis" , 
          dd $ displayDate $ M.machineOperationStartDate machine' ] ]
  machineBoxes = map machineBox machines'
  in main [
    section $ let
      editButton = let
        editButtonBody = [G.pencil, HR.text2DOM " Editovat"]
        editButtonHandler _ = modify var (\appState ->
          appState {
            D.navigation = case D.navigation appState of
              cd @ (D.CompanyDetail _ _ _ _) -> cd { D.editing = True }
              _ -> D.navigation appState })
        editButtonProps = BTN.buttonProps {BTN.onClick = Defined editButtonHandler}
        in BTN.button' editButtonProps editButtonBody
      headerDisplay = h1 $ pack $ C.companyName company'
      headerSet newHeader = let
        company'' = company' {
          C.companyName = unpack $ newHeader }
        in setCompany company''
      header = editable editing' headerDisplay (pack $ C.companyName company') headerSet
      saveHandler _ = saveHandler'
      saveEditButton' = BTN.button' (BTN.buttonProps {
        BTN.onClick = Defined saveHandler , 
        BTN.bsStyle = Defined "primary" }) "Uložit"
      saveEditButton = if editing'
        then [saveEditButton']
        else []
      companyBasicInfo = [
        header , 
        dl $ [
          dt "Označení" , 
          dd $ let
            plantDisplay = text2DOM $ pack $ C.companyPlant company'
            setCompanyPlant companyPlant' = let
              modifiedCompany = company' {
                C.companyPlant = unpack companyPlant' }
              in setCompany modifiedCompany
            in editable editing' plantDisplay (pack $ C.companyPlant company') setCompanyPlant , 
          dt "Adresa" , 
          dd "" , 
          dt "Kontakt" , 
          dd "" , 
          dt "Telefon" , 
          dd "" ] ++ saveEditButton ]
      companyBasicInfo' = if editing' then companyBasicInfo else editButton:companyBasicInfo
      in B.jumbotron companyBasicInfo' , 
    section $ B.grid [
      B.row $
        B.col (B.mkColProps 12) $
          B.panel $
            span $ R.link 
              "Historie servisů"
              (R.maintenances companyId)
              router
      , B.row (machineBoxes ++ [
        let
          buttonProps = BTN.buttonProps {
            BTN.onClick = Defined $ const $
              R.navigate (R.newMachine companyId) router }
          button = BTN.button' buttonProps [G.plus, text2DOM "Přidat zařízení"]
          in B.col (B.mkColProps 4) $ B.panel $ h2 $ button ])
      , B.row $
        B.col (B.mkColProps 12) $
          B.panel $
            span $ R.link
              "Naplánovat servis"
              (R.newMaintenance companyId)
              router ] ]
