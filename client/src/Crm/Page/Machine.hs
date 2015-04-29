{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Page.Machine (
  machineNew ,
  machineDetail ) where

import "fay-base" Data.Text (fromString, pack, (<>), unpack, Text, showInt)
import "fay-base" Prelude hiding (div, span, id)
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
import qualified Crm.Shared.ContactPerson as CP
import qualified Crm.Shared.UpkeepSequence as US
import qualified Crm.Shared.PhotoMeta as PM
import qualified Crm.Shared.Photo as P
import qualified Crm.Shared.Upkeep as U
import qualified Crm.Shared.UpkeepMachine as UM
import qualified Crm.Shared.Employee as E

import qualified Crm.Data.MachineData as MD
import qualified Crm.Data.Data as D
import qualified Crm.Component.DatePicker as DP
import Crm.Component.Form (formRow, editingTextarea, editingInput,
  formRowCol, saveButtonRow, editDisplayRow, row')
import Crm.Server (createMachine, updateMachine, uploadPhotoData, uploadPhotoMeta, getPhoto, deleteMachine)
import Crm.Helpers (parseSafely, displayDate, lmap, rmap, 
  getFileList, fileListElem, fileType, fileName)
import qualified Crm.Router as R

machineDetail :: Bool
              -> Var D.AppState
              -> R.CrmRouter
              -> C.CompanyId
              -> DP.DatePicker
              -> (M.Machine, Text, Text, Text)
              -> (MT.MachineType, [US.UpkeepSequence])
              -> M.MachineId
              -> YMD.YearMonthDay
              -> [(P.PhotoId, PM.PhotoMeta)]
              -> [(U.UpkeepId, U.Upkeep, UM.UpkeepMachine, Maybe E.Employee)]
              -> DOMElement
machineDetail editing appVar router companyId calendarOpen (machine, initialMileageRaw, 
    mileagePerYearRaw, datePickerText) machineTypeTuple machineId nextService photos upkeeps = 

  machineDisplay editing pageHeader button appVar calendarOpen (machine, initialMileageRaw,
      mileagePerYearRaw, datePickerText) machineTypeTuple extraRows extraGrid Nothing []
    where
      pageHeader = if editing then "Editace kompresoru" else "Kompresor"
      extraRow = [editDisplayRow False "Další servis" (displayDate nextService)]
      upkeepHistoryHtml = let
        mkUpkeepRows :: (U.UpkeepId, U.Upkeep, UM.UpkeepMachine, Maybe E.Employee) -> [DOMElement]
        mkUpkeepRows (_, upkeep, upkeepMachine, maybeEmployee) = let
          (labelClass, labelText) = if U.upkeepClosed upkeep
            then ("label-success", "Uzavřený")
            else ("label-warning", "Naplánovaný")
          stateLabel = B.col (B.mkColProps 1) $ span' (class'' ["label", labelClass]) labelText
          date = B.col (B.mkColProps 2) $ displayDate $ U.upkeepDate upkeep
          mthHeader = B.col (B.mkColProps 3) $ strong "Naměřené motohodiny"
          mth = B.col (B.mkColProps 1) $ showInt $ UM.recordedMileage upkeepMachine
          warrantyHeader = B.col (B.mkColProps 1) $ strong "Záruka"
          warranty = B.col (B.mkColProps 1) (if UM.warrantyUpkeep upkeepMachine then "Ano" else "Ne")
          employeeHeader = B.col (B.mkColProps 1) $ strong "Servisák"
          employee = B.col (B.mkColProps 2) $ case maybeEmployee of
            Just employee' -> [text2DOM $ pack $ E.name employee']
            Nothing -> []
          descriptionHeader = B.col (B.mkColProps 2) $ strong "Popis práce"
          description = B.col (B.mkColProps 4) $ pack $ U.workDescription upkeep
          noteHeader = B.col (B.mkColProps 2) $ strong "Poznámka"
          note = B.col (B.mkColProps 4) $ pack $ UM.upkeepMachineNote upkeepMachine
          in [
            B.row [stateLabel, date, mthHeader, mth, 
              warrantyHeader, warranty, employeeHeader, employee] ,
            div' (class'' ["row", "last"]) [descriptionHeader, description, noteHeader, note]]
        rows = (map mkUpkeepRows upkeeps)
        flattenedRows = foldl (++) [] rows
        in if null flattenedRows
          then []
          else (B.row (B.col (B.mkColProps 12) $ h3 "Předchozí servisy")) : flattenedRows
      extraGrid = (if editing && (not $ null upkeeps) 
        then Nothing 
        else (Just $ B.grid upkeepHistoryHtml))
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

      deleteMachineRow = formRow "Smazat" $ BTN.button'
        (BTN.buttonProps { 
          BTN.bsStyle = Defined "danger" ,
          BTN.onClick = Defined $ const $ deleteMachine machineId $ R.navigate (R.companyDetail companyId) router })
        "Smazat"

      extraRows = (case (editing, photos) of
        (True, _) -> [photoUploadRow]
        (_, []) -> []
        _ -> [photoCarouselRow]) ++ extraRow ++ (if editing && null upkeeps
          then [deleteMachineRow]
          else [])
      setEditing :: Bool -> Fay ()
      setEditing editing' = modify appVar (\appState -> appState {
        D.navigation = case D.navigation appState of
          D.MachineScreen (MD.MachineData a b c c1 c2 (Left (MD.MachineDetail d e _ f g i j))) ->
            D.MachineScreen (MD.MachineData a b c c1 c2 (Left (MD.MachineDetail d e editing' f g i j)))
          _ -> D.navigation appState })
      editButtonRow =
        div' (class' "col-md-3") $
          BTN.button'
            (BTN.buttonProps { BTN.onClick = Defined $ const $ setEditing True })
            "Jdi do editačního módu"
      editMachineAction = updateMachine machineId machine (
        setEditing False)
      saveButtonRow' = saveButtonRow "Edituj" editMachineAction
      button = if editing then saveButtonRow' else editButtonRow

machineNew :: R.CrmRouter
           -> Var D.AppState
           -> DP.DatePicker
           -> (M.Machine, Text, Text, Text)
           -> C.CompanyId
           -> (MT.MachineType, [US.UpkeepSequence])
           -> Maybe MT.MachineTypeId
           -> Maybe CP.ContactPersonId
           -> [(CP.ContactPersonId, CP.ContactPerson)]
           -> DOMElement
machineNew router appState datePickerCalendar (machine', initialMileageRaw, mileagePerYearRaw, 
    datePickerText) companyId machineTypeTuple machineTypeId contactPersonId contactPersons = 
  machineDisplay True "Nový kompresor - fáze 2 - specifické údaje o kompresoru"
    buttonRow appState datePickerCalendar (machine', initialMileageRaw, 
      mileagePerYearRaw, datePickerText) machineTypeTuple [] Nothing contactPersonId contactPersons
    where
      machineTypeEither = case machineTypeId of
        Just(machineTypeId') -> MT.MyInt $ MT.getMachineTypeId machineTypeId'
        Nothing -> MT.MyMachineType machineTypeTuple
      saveNewMachine = createMachine machine' companyId machineTypeEither 
        (R.navigate R.defaultFrontPage router)
      buttonRow = saveButtonRow "Vytvoř" saveNewMachine

machineDisplay :: Bool -- ^ true editing mode false display mode
               -> Text -- ^ header of the page
               -> DOMElement
               -> Var D.AppState
               -> DP.DatePicker
               -> (M.Machine, Text, Text, Text) -- ^ machine, _, _, text of the datepicker
               -> (MT.MachineType, [US.UpkeepSequence])
               -> [DOMElement]
               -> Maybe DOMElement
               -> Maybe (CP.ContactPersonId)
               -> [(CP.ContactPersonId, CP.ContactPerson)]
               -> DOMElement
machineDisplay editing pageHeader buttonRow appVar operationStartCalendar (machine',
    initialMileageRaw, mileagePerYearRaw, datePickerText) (machineType, 
    upkeepSequences) extraRows extraGrid contactPersonId contactPersons = let

  changeNavigationState :: (MD.MachineData -> MD.MachineData) -> Fay ()
  changeNavigationState fun = modify appVar (\appState -> appState {
    D.navigation = case D.navigation appState of 
      (D.MachineScreen (md @ (MD.MachineData _ _ _ _ _ _))) -> D.MachineScreen $ fun md
      _ -> D.navigation appState })

  setMachine :: M.Machine -> Fay ()
  setMachine machine = changeNavigationState 
    (\md -> md { MD.machine = (machine, initialMileageRaw, mileagePerYearRaw, datePickerText) })
  
  setMachineFull :: (M.Machine, Text, Text, Text) -> Fay ()
  setMachineFull quadruple = changeNavigationState
    (\md -> md { MD.machine = quadruple })

  datePicker = let
    setDatePickerDate date = changeNavigationState (\state ->
      state { MD.operationStartCalendar = 
        lmap (const date) (MD.operationStartCalendar state) })
    setPickerOpenness openness = changeNavigationState (\state ->
      state { MD.operationStartCalendar = 
        rmap (const openness) (MD.operationStartCalendar state) })
    displayedDate = case M.machineOperationStartDate machine' of
      Just date' -> Right date'
      Nothing -> Left datePickerText
    setDate date = case date of
      Right ymd -> let
        newMachine = machine' { M.machineOperationStartDate = Just ymd }
        in setMachine newMachine
      Left text' -> let 
        newMachine = machine' { M.machineOperationStartDate = Nothing }
        in setMachineFull (newMachine, initialMileageRaw, mileagePerYearRaw, text')
    in DP.datePicker editing operationStartCalendar setDatePickerDate 
      setPickerOpenness displayedDate setDate

  elements = div $ [form' (mkAttrs { className = Defined "form-horizontal" }) $
    B.grid $ [
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
        formRow "Kontaktní osoba" (let
          noContactPersonLabel = "---"
          selectedEmployeeName = maybe noContactPersonLabel (\cpId -> let
            contactPersonFoundInList = lookup cpId contactPersons
            in maybe noContactPersonLabel (pack . CP.name) contactPersonFoundInList) contactPersonId
          selectCpLink cpId c = let
            selectCpAction = changeNavigationState (\md -> md { MD.contactPersonId = cpId })
            in A.a''' (click selectCpAction) (pack $ CP.name c)
          withNoCp = (Nothing, CP.newContactPerson { CP.name = unpack noContactPersonLabel }) : (map (lmap Just) contactPersons)
          selectElements = map (\(cId,c) -> li $ selectCpLink cId c) withNoCp
          buttonLabel = [ text2DOM $ selectedEmployeeName <> " " , span' (class' "caret") "" ]
          in BD.buttonDropdown buttonLabel selectElements ) ,
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
          ("Datum uvedení do provozu") 
          datePicker ,
        row'
          editing
          "Úvodní stav motohodin"
          (unpack initialMileageRaw)
          (eventValue >=> (\rawInitialMileage' -> case parseSafely rawInitialMileage' of
            Just int -> setMachineFull (machine' { M.initialMileage = int }, 
              rawInitialMileage', mileagePerYearRaw, datePickerText)
            Nothing -> setMachineFull (machine', rawInitialMileage', mileagePerYearRaw, datePickerText))) ,
        formRowCol 
          "Provoz mth/rok (Rok má 8760 mth)" [
          (div' (class' "col-md-3") 
            (editingInput 
              (unpack mileagePerYearRaw)
              (eventValue >=> (\rawMileagePerYear' -> case parseSafely rawMileagePerYear' of
                Just int -> setMachineFull (machine' { M.mileagePerYear = int }, 
                  initialMileageRaw, rawMileagePerYear', datePickerText)
                Nothing -> setMachineFull (machine', initialMileageRaw, rawMileagePerYear', datePickerText)))
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
                selectAction = setMachineFull (machine' { M.mileagePerYear = value }, "", showInt value, datePickerText)
                in li $ A.a''' (click selectAction) selectLabel) operationTypeTuples
              buttonLabel' = [text2DOM $ buttonLabel <> " " , span' (class' "caret") ""]
              in BD.buttonDropdown' editing buttonLabel' selectElements)) ] ,
        formRow
          "Poznámka" 
          (editingTextarea (M.note machine') ((\str -> setMachine $ machine' { 
            M.note = str } ) <=< eventString) editing False) ] ++ extraRows ++ [
        div' (class' "form-group") buttonRow ]]] ++ (case extraGrid of
          Just extraGrid' -> [extraGrid']
          Nothing -> [])
  in elements
