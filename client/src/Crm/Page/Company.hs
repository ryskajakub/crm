{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Crm.Page.Company (
  companiesList , 
  companyDetail , 
  companyNew ) where

import           Data.Text                        (fromString, length, (<>))
import qualified Data.Text                        as T
import           Prelude                          hiding (div, span, id, length)
import           Data.Var                         (Var, modify)
import           Data.Maybe                       (onJust)
import           FFI                              (Defined(Defined, Undefined))

import           HaskellReact                     as HR
import qualified HaskellReact.Bootstrap           as B
import qualified HaskellReact.Bootstrap.Button    as BTN
import qualified HaskellReact.Bootstrap.Glyphicon as G
import qualified HaskellReact.Bootstrap.Nav       as BN
import qualified HaskellReact.BackboneRouter      as BR
import           GoogleMaps                       (computeCoordinates)

import qualified Crm.Shared.Company               as C
import qualified Crm.Shared.ContactPerson         as CP
import qualified Crm.Shared.Machine               as M
import qualified Crm.Shared.MachineType           as MT
import qualified Crm.Shared.YearMonthDay          as YMD
import qualified Crm.Shared.Direction             as DIR
import qualified Crm.Shared.Upkeep                as U
import qualified Crm.Shared.MachineKind           as MK

import qualified Crm.Data.Data                    as D
import           Crm.Component.Form               as F
import           Crm.Server                       (createCompany, updateCompany, deleteCompany)
import qualified Crm.Router                       as R
import           Crm.Helpers
import           Crm.Page.ContactPerson           (contactPersonsList')


companiesList :: 
  R.CrmRouter -> 
  C.OrderType -> 
  DIR.Direction -> 
  [(C.CompanyId, C.Company, Maybe YMD.YearMonthDay, [MK.MachineKindEnum])] -> 
  DOMElement
companiesList router orderType direction companies' = let

  mkOrderingLink :: C.OrderType -> DIR.Direction -> DOMElement -> DOMElement
  mkOrderingLink orderType' direction' icon = 
    span' (click $ R.navigate (R.frontPage orderType' direction') router) icon

  (companyNameOrdering, nextServiceOrdering) = case (orderType, direction) of
    (C.CompanyName, DIR.Asc) -> 
      (mkOrderingLink C.CompanyName DIR.Desc G.sortByAlphabet ,
        mkOrderingLink C.NextService DIR.Asc G.sort)
    (C.CompanyName, DIR.Desc) -> 
      (mkOrderingLink C.CompanyName DIR.Asc G.sortByAlphabetAlt ,
        mkOrderingLink C.NextService DIR.Asc G.sort)
    (C.NextService, DIR.Asc) -> 
      (mkOrderingLink C.CompanyName DIR.Asc G.sort,
        mkOrderingLink C.NextService DIR.Desc G.sortByAlphabet)
    (C.NextService, DIR.Desc) -> 
      (mkOrderingLink C.CompanyName DIR.Asc G.sort,
        mkOrderingLink C.NextService DIR.Asc G.sortByAlphabetAlt)

  head' =
    thead $ tr [
      th [text2DOM "Název firmy ", companyNameOrdering ], 
      th "Adresa" , 
      th [text2DOM "Další servis ", nextServiceOrdering ]]
  body = tbody $ map (\idCompany ->
    let 
      (id', company', nextServiceDate, machineKinds) = idCompany
      kindsAsLetters = intersperse (text2DOM " ") . map 
        (span' (class'' ["label", "label-default"]) . T.take 1 . MK.kindToStringRepr) $ machineKinds
    in tr [
      td [
        R.link
          (C.companyName company')
          (R.companyDetail id')
          router , 
        div' (class' "goRight") kindsAsLetters ] ,
      td $ C.companyAddress company' , 
      td $ maybe "" displayDate' nextServiceDate
    ]) companies'
  advice = [ 
    p "Základní stránka aplikace, která ti zodpoví otázku kam jet na servis. Řadí firmy podle toho, kam je třeba jet nejdříve." ,
    p "Klikem na šipky v záhlaví tabulky můžeš řadit tabulku" ]
  pageInfo' = pageInfo "Seznam firem, další servis" $ Just advice
  in B.grid $ B.row $ (pageInfo' ++ withSection [
    let
      buttonProps = BTN.buttonProps {
        BTN.onClick = Defined $ const $ R.navigate R.newCompany router }
      button = BTN.button' buttonProps [
        G.plus , 
        text2DOM " Přidat firmu" ]
      in BN.nav . (:[]) . inNav $ button ,
    B.table [
      head' , 
      body ]])

withSection :: [DOMElement] -> [DOMElement]
withSection elements = map (\element -> section' (class' "col-md-12") element) elements

companyNew :: 
  R.CrmRouter -> 
  Var D.AppState -> 
  C.Company -> 
  DOMElement
companyNew router var company' = let
  saveHandler =
    computeCoordinates (C.companyAddress company') $ \coordinates ->
      createCompany company' (C.mkCoordinates `onJust` coordinates) (\companyId ->
        R.navigate (R.companyDetail companyId) router) router
  setCompany modifiedCompany = modify var (\appState -> appState {
    D.navigation = case D.navigation appState of
      cd @ (D.CompanyNew _) -> cd { D.company = modifiedCompany }
      _ -> D.navigation appState })
  in section $
    (B.grid $ B.row $ B.col (B.mkColProps 12) $ h2 "Nová firma") :
    companyForm Editing var setCompany company' saveHandler []

companyDetail :: 
  InputState -> -- ^ is the page editing mode
  R.CrmRouter -> -- ^ common read data
  Var D.AppState -> -- ^ app state var, where the editing result can be set
  [CP.ContactPerson'] ->
  (C.CompanyId, C.Company) -> -- ^ company, which data are displayed on this screen
  [(M.MachineId, M.Machine, C.CompanyId, MT.MachineTypeId, 
    MT.MachineType, Maybe CP.ContactPerson, Maybe YMD.YearMonthDay, Maybe U.Upkeep)] -> -- ^ machines of the company
  Maybe U.Upkeep ->
  DOMElement -- ^ company detail page fraction
companyDetail editing' router var contactPersons (companyId, company') machines' lastUpkeep' = let

  saveHandler = computeCoordinates (C.companyAddress company') $ \coordinates ->
    updateCompany companyId company' (C.mkCoordinates `onJust` coordinates)
      BR.refresh router

  setCompany modifiedCompany = modify var (\appState -> appState {
    D.navigation = case D.navigation appState of
      cd @ (D.CompanyDetail {}) -> cd { D.company = modifiedCompany }
      _ -> D.navigation appState })

  mkMachineBox (machineId', machine', _, _, machineType, contactPerson, nextService, lastUpkeep) = let 
    (nextServiceRow, healthColor) = maybe ([], "#000") (\nextService' -> 
      ([dt "Další servis" , dd . displayDate' $ nextService'], "#" <> computeColor nextService')) nextService

    mkNonEmptyListElement term value = if T.null value
      then []
      else [
        dt term ,
        dd value ]

    in B.col (B.mkColProps 4) $
      B.panel [
        h3 [
          R.link
            (displayMachine machine' machineType)
            (R.machineDetail machineId')
            router ,
          span' (class' "quick-upkeep-link") $
            R.link 
              G.wrench
              (R.newMaintanceViaQuickLink companyId machineId')
              router ,
          span' ((class' "health") { style = Defined . Style $ healthColor }) "•" ] ,
        dl $ 
          (mkNonEmptyListElement "Označení" (M.label_ $ machine')) ++ 
          (mkNonEmptyListElement "Druh" (MK.kindToStringRepr . MT.kind $ machineType)) ++
          (mkNonEmptyListElement "Uvedení do provozu" (maybe "" displayDate (M.machineOperationStartDate machine'))) ++
          (mkNonEmptyListElement "Výrobní číslo" (M.serialNumber machine')) ++
          (mkNonEmptyListElement "Označení" (M.label_ machine')) ++
          (mkNonEmptyListElement "Rok výroby" (M.yearOfManufacture machine')) ++
          (mkNonEmptyListElement "Kontaktní osoba" (maybe "" CP.name contactPerson)) ++
          (mkNonEmptyListElement "Poslední servis" (maybe "" (displayDate . U.upkeepDate) lastUpkeep)) ++
          nextServiceRow ]
  machineBoxes = map mkMachineBox machines'

  deleteButton = BTN.button' (BTN.buttonProps {
    BTN.onClick = Defined $ const $ deleteCompany companyId (R.navigate R.defaultFrontPage router) router ,
    BTN.disabled = if null machines' then Undefined else Defined True ,
    BTN.bsStyle = Defined "danger" }) "Smazat"

  companyFormSection :: [DOMElement]
  companyFormSection = companyForm editing' var setCompany company' saveHandler [deleteButton]
  machineBoxItems = machineBoxes ++ [ let
    buttonProps = BTN.buttonProps {
      BTN.onClick = Defined $ const $
        R.navigate (R.newMachinePhase1 companyId) router }
    button = BTN.button' buttonProps [G.plus, text2DOM "Přidat zařízení"]
    in B.col (B.mkColProps 4) $ B.panel $ h2 $ button ]

  machineBoxItems' = foldl (\acc (i, e) -> 
    if i `mod` 3 == 0
      then acc ++ [[e]]
      else let
        unmodifiedList = init acc
        appendedToLast = last acc ++ [e]
        in unmodifiedList ++ [appendedToLast]
    ) [] (zipWithIndex machineBoxItems)

  machineBoxItemsHtml :: [DOMElement]
  machineBoxItemsHtml = map B.row machineBoxItems'

  lastUpkeepRecommendation :: [DOMElement]
  lastUpkeepRecommendation = maybe [] (\lastUpkeep -> (:[]) . B.row $ B.col (B.mkColProps 12) [
    h2 $ "Doporučení z posledního servisu - " <> (displayDate . U.upkeepDate $ lastUpkeep) ,
    p . U.recommendation $ lastUpkeep ]) lastUpkeep'

  contactPersonsHtml :: DOMElement
  contactPersonsHtml = contactPersonsList' router contactPersons

  headerNavigSection :: DOMElement
  headerNavigSection = B.grid [ B.row $ B.col (B.mkColProps 12) $ 
    h2 (case editing' of Editing -> "Editace firmy"; _ -> "Firma") ,
    (B.row $ B.col (B.mkColProps 12) $ BN.nav [
      R.link "Historie servisů" (R.maintenances companyId) router ,
      form' (class' "navbar-form") $
        BTN.button' (BTN.buttonProps {
          BTN.disabled = Defined $ if null machines' then True else False ,
          BTN.onClick = Defined $ const $ R.navigate (R.newMaintenance companyId) router })
          [G.plus, text2DOM " Naplánovat servis" ] ,
      form' (class' "navbar-form") $
        BTN.button' (BTN.buttonProps {
          BTN.onClick = Defined $ const $ R.navigate (R.newContactPerson companyId) router })
          [G.plus, text2DOM " Přidat kontaktní osobu" ] ,
      R.link "Kontaktní osoby" (R.contactPersonList companyId) router ,
      R.link "Schéma zapojení" (R.machinesSchema companyId) router ])]

  in div $ headerNavigSection : companyFormSection ++ [B.grid $ contactPersonsHtml : lastUpkeepRecommendation ++ machineBoxItemsHtml ]

companyForm :: 
  InputState -> -- ^ is the page editing mode
  Var D.AppState -> -- ^ app state var, where the editing result can be set
  (C.Company -> Fay ()) -> -- ^ modify the edited company data
  C.Company -> -- ^ company, whose data are displayed on this screen
  Fay () -> -- ^ handler called when the user hits save
  [DOMElement] -> -- ^ delete button
  [DOMElement] -- ^ company detail page fraction
companyForm editing' var setCompany company' saveHandler' deleteButton = let
  editButton = let
    editButtonBody = [G.pencil, HR.text2DOM " Editovat"]
    editButtonHandler _ = modify var (\appState ->
      appState {
        D.navigation = case D.navigation appState of
          cd @ (D.CompanyDetail {}) -> cd { D.editing = Editing }
          _ -> D.navigation appState })
    editButtonProps = BTN.buttonProps {BTN.onClick = Defined editButtonHandler}
    in BTN.button' editButtonProps editButtonBody
  headerSet newHeader = let
    company'' = company' {
      C.companyName = newHeader }
    in setCompany company''

  appliedInput = F.input editing' True

  header = let
    input' = appliedInput (SetValue $ C.companyName company') headerSet
    in case editing' of
      Editing -> dl [
        dt "Jméno firmy" ,
        dd input' ]
      Display -> h1 $ C.companyName company'

  validationMessages = if (length $ C.companyName company') > 0
    then []
    else ["Název firmy musí mít alespoň jeden znak."]
  validationHtml' = validationHtml validationMessages

  saveEditButton' = BTN.button' (BTN.buttonProps {
    BTN.onClick = Defined $ const saveHandler' , 
    BTN.disabled = if null validationMessages then Undefined else Defined True ,
    BTN.bsStyle = Defined "primary" }) "Uložit"
  saveEditButton = case editing' of
    Editing -> div' (class'' ["company", "company-save"]) $ saveEditButton' : deleteButton
    Display -> text2DOM ""

  companyBasicInfo = [
    header , 
    dl $ [
      dt "Adresa (ulice, město, PSČ)" , 
      dd $ appliedInput
        (SetValue $ C.companyAddress company')
        (\text -> setCompany (company' { C.companyAddress = text })) ,
      dt "Poznámka" , 
      dd $ F.textarea' 3 editing' True
        (SetValue $ C.companyNote company') 
        (\text -> setCompany (company' { C.companyNote = text })) ]
      ++ [saveEditButton] ]
  companyBasicInfo' = case editing' of 
    Editing -> companyBasicInfo 
    Display -> editButton:companyBasicInfo
  in (B.grid $ B.row $ B.col (B.mkColProps 12) $ B.jumbotron companyBasicInfo') : validationHtml' : []
