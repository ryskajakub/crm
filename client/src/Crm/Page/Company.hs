{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Page.Company (
  companiesList , 
  companyDetail , 
  companyNew ) where

import "fay-base" Data.Text (fromString, unpack, pack, Text) 
import "fay-base" Prelude hiding (div, span, id)
import Data.Var (Var, modify)
import FFI (Defined(Defined))

import HaskellReact as HR
import qualified HaskellReact.Bootstrap as B
import qualified HaskellReact.Bootstrap.Button as BTN
import qualified HaskellReact.Bootstrap.Glyphicon as G

import qualified Crm.Shared.Company as C
import qualified Crm.Shared.Machine as M
import qualified Crm.Shared.MachineType as MT
import qualified Crm.Shared.YearMonthDay as YMD
import qualified Crm.Data as D
import Crm.Component.Editable (editable, editablePlain, editable')
import Crm.Server (createCompany, updateCompany)
import qualified Crm.Router as R
import Crm.Helpers (displayDate)

companiesList :: R.CrmRouter
              -> [(C.CompanyId, C.Company, Maybe YMD.YearMonthDay)]
              -> DOMElement
companiesList router companies' = let
  head' =
    thead $ tr [
      th "Název firmy" , 
      th "Označení (Provozovna)" , 
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
  in main [
    section $
      let
        buttonProps = BTN.buttonProps {
          BTN.onClick = Defined $ const $ R.navigate R.newCompany router }
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
  saveHandler = createCompany company' (R.navigate R.frontPage router)
  setCompany modifiedCompany = modify var (\appState -> appState {
    D.navigation = case D.navigation appState of
      cd @ (D.CompanyNew _) -> cd { D.company = modifiedCompany }
      _ -> D.navigation appState })
  in companyForm editing' var setCompany company' saveHandler

companyDetail :: Bool -- ^ is the page editing mode
              -> R.CrmRouter -- ^ common read data
              -> Var D.AppState -- ^ app state var, where the editing result can be set
              -> (C.CompanyId, C.Company) -- ^ company, which data are displayed on this screen
              -> [(M.MachineId, M.Machine, C.CompanyId, MT.MachineTypeId, MT.MachineType)] 
                 -- ^ machines of the company
              -> DOMElement -- ^ company detail page fraction
companyDetail editing' router var (companyId, company') machines' = let
  saveHandler = do
    updateCompany companyId company'
    R.navigate R.frontPage router
  setCompany modifiedCompany = modify var (\appState -> appState {
    D.navigation = case D.navigation appState of
      cd @ (D.CompanyDetail _ _ _ _) -> cd { D.company = modifiedCompany }
      _ -> D.navigation appState })
  machineBox (machineId', machine', _, _, machineType) =
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
  companyFormSection = companyForm editing' var setCompany company' saveHandler
  upkeepsHistoryRow = B.row $
    B.col (B.mkColProps 12) $
      B.panel $
        span $ R.link 
          "Historie servisů"
          (R.maintenances companyId)
          router
  machineBoxesRow = B.row (machineBoxes ++ [ let
    buttonProps = BTN.buttonProps {
      BTN.onClick = Defined $ const $
        R.navigate (R.newMachine companyId) router }
    button = BTN.button' buttonProps [G.plus, text2DOM "Přidat zařízení"]
    in B.col (B.mkColProps 4) $ B.panel $ h2 $ button ])
  planUpkeepRow = B.row $
    B.col (B.mkColProps 12) $
      B.panel $
        span $ R.link
          "Naplánovat servis"
          (R.newMaintenance companyId)
          router
  in main [
    companyFormSection ,
    section $ B.grid [
      upkeepsHistoryRow ,
      machineBoxesRow ,
      planUpkeepRow ]]

companyForm :: Bool -- ^ is the page editing mode
            -> Var D.AppState -- ^ app state var, where the editing result can be set
            -> (C.Company -> Fay ()) -- ^ modify the edited company data
            -> C.Company -- ^ company, which data are displayed on this screen
            -> Fay () -- ^ handler called when the user hits save
            -> DOMElement -- ^ company detail page fraction
companyForm editing' var setCompany company' saveHandler' = 
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
    inputWrapper input = dl [
      dt "Jméno firmy" ,
      dd input ]
    header = editable' Nothing inputWrapper editing' headerDisplay (pack $ C.companyName company') headerSet
    saveHandler _ = saveHandler'
    saveEditButton' = BTN.button' (BTN.buttonProps {
      BTN.onClick = Defined saveHandler , 
      BTN.bsStyle = Defined "primary" }) "Uložit"
    saveEditButton = if editing'
      then [saveEditButton']
      else []
    display :: String -> Text
    display string = case string of
      s | pack s == "" -> "-"
      s -> pack s
    hereEditablePlain = editablePlain editing'
    companyBasicInfo = [
      header , 
      dl $ [
        dt "Označení provozovny (pro odlišení provozoven se stejným názvem firmy)" , 
        dd $ hereEditablePlain
          (display $ C.companyPlant company') 
          (\text -> setCompany (company' { C.companyPlant = unpack text })) , 
        dt "Adresa" , 
        dd $ hereEditablePlain
          (display $ C.companyAddress company')
          (\text -> setCompany (company' { C.companyAddress = unpack text })) , 
        dt "Jméno kontaktní osoby" , 
        dd $ hereEditablePlain
          (display $ C.companyPerson company')
          (\text -> setCompany (company' { C.companyPerson = unpack text })) , 
        dt "Telefon na kontaktní osobu" , 
        dd $ hereEditablePlain
          (display $ C.companyPhone company')
          (\text -> setCompany (company' { C.companyPhone = unpack text })) ]
        ++ saveEditButton ]
    companyBasicInfo' = if editing' then companyBasicInfo else editButton:companyBasicInfo
    in B.jumbotron companyBasicInfo'
