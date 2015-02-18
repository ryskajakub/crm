{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Page.Machine (
  machineNew ,
  machineDetail ) where

import "fay-base" Data.Text (fromString, pack, (<>), unpack, Text)
import "fay-base" Prelude hiding (div, span, id)
import "fay-base" Data.Maybe (whenJust)
import "fay-base" Data.Var (Var, modify)
import "fay-base" FFI (Defined(Defined))

import HaskellReact as HR
import qualified HaskellReact.Bootstrap as B
import qualified HaskellReact.Bootstrap.Button as BTN
import qualified HaskellReact.Bootstrap.ButtonDropdown as BD
import qualified HaskellReact.Jasny as J
import qualified HaskellReact.Tag.Hyperlink as A
import qualified HaskellReact.Tag.Image as IMG
import HaskellReact.Bootstrap.Carousel (carousel)

import qualified JQuery as JQ

import qualified Crm.Shared.Machine as M
import qualified Crm.Shared.YearMonthDay as YMD
import qualified Crm.Shared.MachineType as MT
import qualified Crm.Shared.Company as C
import qualified Crm.Shared.UpkeepSequence as US
import qualified Crm.Shared.PhotoMeta as PM
import qualified Crm.Shared.Photo as P

import qualified Crm.Data.MachineData as MD
import qualified Crm.Data.Data as D
import qualified Crm.Component.DatePicker as DP
import Crm.Component.Form (formRow, editingTextarea, editingInput,
  formRowCol, saveButtonRow, editDisplayRow, row')
import Crm.Server (createMachine, updateMachine, uploadPhotoData, uploadPhotoMeta, getPhoto)
import Crm.Helpers (parseSafely, displayDate, lmap, rmap, eventInt, 
  getFileList, fileListElem, fileType, fileName)
import Crm.Router (CrmRouter, navigate, defaultFrontPage)

machineDetail :: Bool
              -> Var D.AppState
              -> DP.DatePicker
              -> M.Machine
              -> MT.MachineTypeId
              -> (MT.MachineType, [US.UpkeepSequence])
              -> M.MachineId
              -> YMD.YearMonthDay
              -> [(P.PhotoId, PM.PhotoMeta)]
              -> (DOMElement, Fay ())
machineDetail editing appVar calendarOpen machine machineTypeId machineTypeTuple 
    machineId nextService photos = 
  machineDisplay editing pageHeader button appVar calendarOpen machine machineTypeTuple extraRows
    where
      pageHeader = if editing then "Editace kompresoru" else "Kompresor"
      extraRow = [editDisplayRow False "Další servis" (displayDate nextService)]
      photoUploadRow = editDisplayRow
        True
        "Fotka" 
        (let 
          imageUploadHandler = const $ do
            fileUpload <- JQ.select "#file-upload"
            files <- getFileList fileUpload
            file <- fileListElem 0 files
            type' <- fileType file
            name <- fileName file
            uploadPhotoData file machineId (\photoId ->
              uploadPhotoMeta (PM.PhotoMeta (unpack type') (unpack name)) photoId (return ()))
          imageUploadLabel = "Přidej fotku"
          photoList = map (\(_, photoMeta) -> 
            li' (class' "list-unstyled") [pack $ PM.fileName photoMeta] ) photos
          in div [(ul $ photoList) : [div [
            J.fileUpload ,
            BTN.button'
              (BTN.buttonProps {
                BTN.bsStyle = Defined "primary" ,
                BTN.onClick = Defined imageUploadHandler })
              imageUploadLabel ]]]) 
      mkPhoto (photoId,_) = IMG.image' mkAttrs (IMG.mkImageAttrs $ getPhoto photoId)
      photoCarouselRow = editDisplayRow
        True
        "Fotky"
        (carousel "my-carousel" (map mkPhoto photos))
      extraRows = (case (editing, photos) of
        (True, _) -> [photoUploadRow]
        (_, []) -> []
        _ -> [photoCarouselRow]) ++ extraRow
      setEditing :: Fay ()
      setEditing = modify appVar (\appState -> appState {
        D.navigation = case D.navigation appState of
          D.MachineScreen (MD.MachineData a b c (Left (MD.MachineDetail d e _ f g))) ->
            D.MachineScreen (MD.MachineData a b c (Left (MD.MachineDetail d e True f g)))
          _ -> D.navigation appState })
      editButtonRow =
        div' (class' "col-md-3") $
          BTN.button'
            (BTN.buttonProps { BTN.onClick = Defined $ const setEditing })
            "Jdi do editačního módu"
      editMachineAction = updateMachine machineId machineTypeId machine (return ())
      saveButtonRow' = saveButtonRow "Edituj" editMachineAction
      button = if editing then saveButtonRow' else editButtonRow

machineNew :: CrmRouter
           -> Var D.AppState
           -> DP.DatePicker
           -> M.Machine
           -> C.CompanyId
           -> (MT.MachineType, [US.UpkeepSequence])
           -> Maybe MT.MachineTypeId
           -> (DOMElement, Fay ())
machineNew router appState datePickerCalendar machine' companyId machineTypeTuple machineTypeId = 
  machineDisplay True "Nový kompresor - fáze 2 - specifické údaje o kompresoru" 
      buttonRow appState datePickerCalendar machine' machineTypeTuple []
    where
      machineTypeEither = case machineTypeId of
        Just(machineTypeId') -> MT.MyInt $ MT.getMachineTypeId machineTypeId'
        Nothing -> MT.MyMachineType machineTypeTuple
      saveNewMachine = createMachine machine' companyId machineTypeEither 
        (navigate defaultFrontPage router)
      buttonRow = saveButtonRow "Vytvoř" saveNewMachine

machineDisplay :: Bool -- ^ true editing mode false display mode
               -> Text -- ^ header of the page
               -> DOMElement
               -> Var D.AppState
               -> DP.DatePicker
               -> M.Machine
               -> (MT.MachineType, [US.UpkeepSequence])
               -> [DOMElement]
               -> (DOMElement, Fay ())
machineDisplay editing pageHeader buttonRow appVar operationStartCalendar
    machine' (machineType, upkeepSequences) extraRows = let

  changeNavigationState :: (MD.MachineData -> MD.MachineData) -> Fay ()
  changeNavigationState fun = modify appVar (\appState -> appState {
    D.navigation = case D.navigation appState of 
      (D.MachineScreen (md @ (MD.MachineData _ _ _ _))) -> D.MachineScreen $ fun md
      _ -> D.navigation appState })

  setMachine :: M.Machine -> Fay ()
  setMachine machine = changeNavigationState (\md -> md { MD.machine = machine })
  elements = form' (mkAttrs { className = Defined "form-horizontal" }) $
    B.grid [
      B.row $ B.col (B.mkColProps 12) $ h2 pageHeader ,
      B.row $ [
        row'
          False
          "Typ zařízení" 
          (MT.machineTypeName machineType) 
          (const $ return ()) ,
        row'
          False
          "Výrobce"
          (MT.machineTypeManufacturer machineType)
          (const $ return ()) ,
        row'
          editing
          "Výrobní číslo"
          (M.serialNumber machine')
          (eventString >=> (\s -> setMachine $ machine' { M.serialNumber = s })) ,
        row'
          editing
          "Rok výroby"
          (M.yearOfManufacture machine')
          (eventString >=> (\s -> setMachine $ machine' { M.yearOfManufacture = s })) ,
        editDisplayRow
          editing
          ("Datum uvedení do provozu") (let
            setDatePickerDate date = changeNavigationState (\state ->
              state { MD.operationStartCalendar = 
                lmap (const date) (MD.operationStartCalendar state) })
            setPickerOpenness openness = changeNavigationState (\state ->
              state { MD.operationStartCalendar = 
                rmap (const openness) (MD.operationStartCalendar state) })
            displayedDate = M.machineOperationStartDate machine'
            setDate date = let
              newMachine = machine' { M.machineOperationStartDate = date }
              in setMachine newMachine
            in DP.datePicker editing operationStartCalendar setDatePickerDate setPickerOpenness
              displayedDate setDate) ,
        row'
          editing
          "Úvodní stav motohodin"
          (show $ M.initialMileage machine')
          (let
            setInitialMileage :: Int -> Fay ()
            setInitialMileage int = setMachine $ machine' { M.initialMileage = int }
            in flip whenJust setInitialMileage . parseSafely <=< eventValue ) ,
        formRowCol 
          "Provoz mth/rok (Rok má 8760 mth)" [
          (div' (class' "col-md-3") 
            (editingInput 
              (show $ M.mileagePerYear machine')
              (eventInt (\int -> setMachine $ machine' { M.mileagePerYear = int } ))             
              editing
              True)) ,
          (label' (class'' ["control-label", "col-md-3"]) "Typ provozu") ,
          (div' (class' "col-md-3") 
            (let 
              upkeepPerMileage = minimum repetitions where
                nonOneTimeSequences = filter (not . US.oneTime) upkeepSequences
                repetitions = map US.repetition nonOneTimeSequences
              operationTypeTuples = [(8760, "24/7"), (upkeepPerMileage, "1 za rok")]
              buttonLabelMaybe = find (\(value, _) -> value == M.mileagePerYear machine') 
                operationTypeTuples
              buttonLabel = maybe "Jiný" snd buttonLabelMaybe
              selectElements = map (\(value, selectLabel) -> let
                selectAction = setMachine $ machine' { M.mileagePerYear = value }
                in li $ A.a''' (click selectAction) selectLabel) operationTypeTuples
              buttonLabel' = [text2DOM $ buttonLabel <> " " , span' (class' "caret") ""]
              in BD.buttonDropdown' editing buttonLabel' selectElements)) ] ,
        formRow
          "Poznámka" 
          (editingTextarea (M.note machine') ((\str -> setMachine $ machine' { 
            M.note = str } ) <=< eventString) editing False) ] ++ extraRows ++ [
        div' (class' "form-group") buttonRow ]]
  in (elements, return ())
