{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Crm.Page.UpkeepHistory (
  upkeepHistory ) where

import           Data.Text                     (fromString, showInt, (<>), intercalate, pack)
import           Prelude                       hiding (div, span, id, intercalate)
import           FFI                           (Defined(Defined))
import           Data.Var                         (Var, modify)

import qualified JQuery                        as JQ

import           HaskellReact
import qualified HaskellReact.Bootstrap        as B
import qualified HaskellReact.Bootstrap.Nav    as BN
import qualified HaskellReact.Bootstrap.Button as BTN
import qualified HaskellReact.BackboneRouter   as BR
import qualified HaskellReact.Tag.Image        as IMG
import qualified HaskellReact.Bootstrap.Glyphicon as G

import qualified Crm.Shared.Upkeep             as U
import qualified Crm.Shared.UpkeepMachine      as UM
import qualified Crm.Shared.Machine            as M
import qualified Crm.Shared.MachineType        as MT
import qualified Crm.Shared.MachineKind        as MK
import qualified Crm.Shared.Employee           as E
import qualified Crm.Shared.Company            as C
import qualified Crm.Shared.Photo              as P
import qualified Crm.Shared.Api                as A

import qualified Crm.Data.Data                    as D
import           Crm.Helpers                   (displayDate, renderMarkup, reload)
import           Crm.Router
import           Crm.Server                    (deleteUpkeep, reopenUpkeep, deletePhoto)
import qualified Crm.Runtime                   as Runtime


upkeepHistory :: 
  [(U.UpkeepId, U.Upkeep2Markup, [(UM.UpkeepMachineMarkup, MT.MachineType, M.MachineId)], [E.Employee'], [P.PhotoId])] -> 
  C.CompanyId -> 
  Bool ->
  Var D.AppState ->
  CrmRouter -> 
  (DOMElement, Fay ())
upkeepHistory upkeepsInfo companyId deletable var router = let

  upkeepRenderHtml (upkeepId, upkeep, upkeepMachines, employees, photos) = 
    ([generalUpkeepInfo, notes, upkeepMachinesInfo] ++ photoHtml, fetchPhotos) where

    generalUpkeepInfo = B.row' marginTop [
      B.col (B.mkColProps 4) [
        text2DOM $ (<> " ") $ displayDate $ U.upkeepDate upkeep ,
        span' (class'' ["label", "upkeep-state" , labelClass]) labelText ] ,
      let 
        i = if length employees > 1 then "i" else ""
        in B.col (B.mkColProps 4) [ strong $ "Servisman" <> i <> ": ", text2DOM employeeText ] ,
      B.col (B.mkColProps 2) $ formLink , 
      B.col (B.mkColProps 1) $ editLink ,
      B.col (B.mkColProps 1) $ deleteButton ] where
        employeeText = intercalate " " . map (\(_, employee) -> E.name employee) $ employees
        (labelClass, labelText, formLink) = if U.upkeepClosed upkeep
          then ("label-success", "Uzavřený", let
            buttonProps = (BTN.buttonProps {
              BTN.bsStyle = Defined "warning" ,
              BTN.onClick = Defined $ const clickHandler })
            clickHandler = reopenUpkeep upkeepId goEditUpkeep router
            goEditUpkeep = navigate (upkeepDetail upkeepId) router
            in BTN.button' buttonProps "Otevřít")
          else ("label-warning", "Naplánovaný", link "Uzavřít" (upkeepDetail upkeepId) router) 
        marginTop attributes = let
          previousClassname = className attributes
          newClassname = case previousClassname of
            Defined text -> Defined $ text <> " upkeep-row"
            _ -> Defined "upkeep-row"
          in attributes { className = newClassname }
        editLink = link "Editovat" (upkeepDetail upkeepId) router
        deleteButton = let
          clickHandler = deleteUpkeep upkeepId BR.refresh router
          buttonProps = (BTN.buttonProps {
            BTN.bsStyle = Defined "danger" ,
            BTN.onClick = Defined $ const clickHandler })
          in span' (class' "delete") $ BTN.button' buttonProps "Smazat"

    notes = B.row [
      mkCol note , 
      mkCol recommendation ] where
        mkCol = B.col (B.mkColProps 6)
        note = [
          h3 "Popis servisu" ,
          noteContent ]
        recommendation = [
          h3 "Doporučení" ,
          recommendationContent ]
        noteContent = div . renderMarkup . U.workDescription $ upkeep
        recommendationContent = div . renderMarkup . U.recommendation $ upkeep

    mkLineUpkeepMachineInfo (upkeepMachine, machineType, machineId) =
      B.col (B.mkColProps 6) $ B.panel [ h3 $ link 
        (MT.machineTypeName machineType)
        (machineDetail machineId)
        router ,
        dl [[
          dt "Plánované úkony" ,
          dd . renderMarkup . UM.upkeepMachineNote $ upkeepMachine ,
          dt "Závěry po servisu" ,
          dd . renderMarkup . UM.endNote $ upkeepMachine ] ++ 
          (if U.upkeepClosed upkeep then (if MT.kind machineType == MK.RotaryScrewCompressor then [
          dt "Naměřené motohodiny" ,
          dd $ showInt $ UM.recordedMileage upkeepMachine] else []) ++ [
          dt "Záruka" ,
          dd $ (if UM.warrantyUpkeep upkeepMachine then "Ano" else "Ne") ] else []) ]]
    upkeepMachinesInfo = B.row $ map mkLineUpkeepMachineInfo upkeepMachines
    photoHtml = map mkPhotoRow photos

    mkPhotoRow photoId = let
      deleteButton = let
        buttonProps = (BTN.buttonProps {
          BTN.bsStyle = Defined "danger" ,
          BTN.onClick = Defined $ const clickHandler })
        clickHandler = deletePhoto photoId reload router
        in BTN.button' buttonProps [G.arrowDown, text2DOM " Smazat fotku"]
      in B.row [ 
      B.col ((B.mkColProps 2) { B.mdOffset = Defined 10 }) deleteButton ,
      B.fullCol $ IMG.image' 
        (mkAttrs { id = Defined . (<>) "photo-" . showInt . P.getPhotoId $ photoId})
        ((IMG.mkImageAttrs "") { IMG.width = Defined 1140 }) ]

    fetchPhotos = forM_ photos $ \photoId -> Runtime.passwordAjax
      (pack A.photos <> "/" <> (showInt . P.getPhotoId $ photoId))
      (\imageData -> do
        let photoHTMLId = ((<>) "#photo-" . showInt . P.getPhotoId $ photoId)
        photoHtmlElement <- JQ.select photoHTMLId
        _ <- JQ.setAttr "src" ("data:image/jpeg;base64," <> imageData) photoHtmlElement 
        return ())
      Nothing
      Runtime.get
      Nothing
      Nothing
      router

  upkeepsHtml = map upkeepRenderHtml upkeepsInfo
  flattenedUpkeepsHtml = foldl (++) [] . map fst $ upkeepsHtml
  allFetchPhotos = foldl (>>) (return ()) . map snd $ upkeepsHtml
  
  header = B.row $ B.col (B.mkColProps 12) (h2 "Historie servisů")
  linkToCompany = B.row $ B.col (B.mkColProps 12) $
    BN.nav [ 
      link "Zpátky na firmu" (companyDetail companyId) router ,
      let 
        buttonProps = (BTN.buttonProps {
          BTN.bsStyle = Defined "danger" ,
          BTN.onClick = Defined $ const clickHandler })
        clickHandler = modify var $ \appState -> appState {
          D.navigation = case D.navigation appState of
            dh @ (D.UpkeepHistory {}) -> dh { D.deletable = not deletable }
            _ -> D.navigation appState }
        label = if deletable then "Zakázat smazávání" else "Povolit smazávání"
        in BTN.button' buttonProps label ]
  in (div $ B.grid (header : linkToCompany : flattenedUpkeepsHtml), allFetchPhotos)
