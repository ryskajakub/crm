{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Crm.Page.Machine (
  machineNew ,
  machineDetail ) where

import           Data.Text                             (fromString, (<>), Text, showInt)
import           Prelude                               hiding (div, span, id)
import           Data.Var                              (Var, modify)
import           Data.Maybe                            (onJust)
import           FFI                                   (Defined(Defined))

import           HaskellReact                          hiding (row)
import qualified HaskellReact.Bootstrap                as B
import qualified HaskellReact.Bootstrap.Button         as BTN
import qualified HaskellReact.Bootstrap.ButtonDropdown as BD
import qualified HaskellReact.Jasny                    as J
import qualified HaskellReact.Tag.Hyperlink            as A
import qualified HaskellReact.Tag.Image                as IMG
import           HaskellReact.Bootstrap.Carousel       (carousel)
import qualified HaskellReact.BackboneRouter           as BR
import qualified JQuery                                as JQ

import qualified Crm.Shared.Machine                    as M
import qualified Crm.Shared.YearMonthDay               as YMD
import qualified Crm.Shared.MachineType                as MT
import qualified Crm.Shared.Company                    as C
import qualified Crm.Shared.ContactPerson              as CP
import qualified Crm.Shared.UpkeepSequence             as US
import qualified Crm.Shared.PhotoMeta                  as PM
import qualified Crm.Shared.Photo                      as P
import qualified Crm.Shared.Upkeep                     as U
import qualified Crm.Shared.UpkeepMachine              as UM
import qualified Crm.Shared.ExtraField                 as EF
import qualified Crm.Shared.MachineKind                as MK

import qualified Crm.Data.MachineData                  as MD
import qualified Crm.Data.Data                         as D
import qualified Crm.Component.DatePicker              as DP
import           Crm.Component.Form
import           Crm.Server 
import           Crm.Helpers 
import qualified Crm.Router                            as R
import qualified Crm.Validation                        as V

machineDetail :: InputState
              -> Var D.AppState
              -> R.CrmRouter
              -> C.CompanyId
              -> DP.DatePicker
              -> (M.Machine, Text)
              -> MK.MachineKindEnum
              -> (MT.MachineType, [US.UpkeepSequence])
              -> M.MachineId
              -> YMD.YearMonthDay
              -> [(P.PhotoId, PM.PhotoMeta)]
              -> [(U.UpkeepId, U.Upkeep, UM.UpkeepMachine)]
              -> (CP.ContactPerson, Maybe CP.ContactPersonId, MD.ContactPersonInMachine)
              -> [(CP.ContactPersonId, CP.ContactPerson)]
              -> V.Validation
              -> Maybe M.MachineId
              -> [(M.MachineId, M.Machine)]
              -> [(EF.ExtraFieldId, MK.MachineKindSpecific, Text)]
              -> DOMElement
machineDetail editing appVar router companyId calendarOpen (machine, 
    datePickerText) machineSpecific machineTypeTuple machineId nextService photos upkeeps
    contactPersonId contactPersons v otherMachineId om extraFields =

  machineDisplay editing pageHeader button appVar calendarOpen (machine, 
      datePickerText) machineSpecific machineTypeTuple extraRows extraGrid 
      contactPersonId contactPersons v otherMachineId om extraFields
    where
      pageHeader = case editing of Editing -> "Editace stroje"; _ -> "Stroj"
      extraRow = [editableRow Display "Další servis" (displayDate nextService)]
      upkeepHistoryHtml = let
        mkUpkeepRows :: (U.UpkeepId, U.Upkeep, UM.UpkeepMachine) -> [DOMElement]
        mkUpkeepRows (_, upkeep, upkeepMachine) = let
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
          employee = B.col (B.mkColProps 2) $ "tbd"
          descriptionHeader = B.col (B.mkColProps 2) $ strong "Popis práce"
          description = B.col (B.mkColProps 4) $ U.workDescription upkeep
          noteHeader = B.col (B.mkColProps 2) $ strong "Poznámka"
          note = B.col (B.mkColProps 4) $ UM.upkeepMachineNote upkeepMachine
          in [
            B.row [stateLabel, date, mthHeader, mth, 
              warrantyHeader, warranty, employeeHeader, employee] ,
            div' (class'' ["row", "last"]) [descriptionHeader, description, noteHeader, note]]
        rows = (map mkUpkeepRows upkeeps)
        flattenedRows = foldl (++) [] rows
        in if null flattenedRows
          then []
          else (B.row (B.col (B.mkColProps 12) $ h3 "Předchozí servisy")) : flattenedRows
      extraGrid = (if editing == Editing && (not $ null upkeeps) 
        then Nothing 
        else (Just $ B.grid upkeepHistoryHtml))
      
      editableRowEditing = editableRow editing
    
      photoUploadRow = editableRowEditing
        "Fotka" 
        (let 
          imageUploadHandler = const $ do
            fileUpload <- JQ.select "#file-upload"
            files <- getFileList fileUpload
            file <- fileListElem 0 files
            type' <- fileType file
            name <- fileName file
            uploadPhotoData file machineId $ \photoId ->
              uploadPhotoMeta (PM.PhotoMeta type' name) photoId BR.refresh
          imageUploadLabel = "Přidej fotku"
          photoList = map (\(photoId, photoMeta) -> let
            deletePhotoHandler = const $ deletePhoto photoId BR.refresh
            deletePhotoButton = BTN.button' 
              (BTN.buttonProps {
                BTN.bsStyle = Defined "danger" ,
                BTN.onClick = Defined deletePhotoHandler })
              "Smaž fotku"
            in li [text2DOM $ PM.fileName photoMeta, deletePhotoButton] ) photos
          in div [(ul' (class' "list-unstyled") photoList) : [div [
            J.fileUploadI18n "Vyber obrázek" "Změň" ,
            BTN.button'
              (BTN.buttonProps {
                BTN.bsStyle = Defined "primary" ,
                BTN.onClick = Defined imageUploadHandler })
              imageUploadLabel ]]]) 
      mkPhoto (photoId,_) = IMG.image' mkAttrs (IMG.mkImageAttrs $ fetchPhoto photoId)
      photoCarouselRow = editableRowEditing
        "Fotky"
        (carousel "my-carousel" (map mkPhoto photos))

      deleteMachineRow = oneElementRow "Smazat" $ BTN.button'
        (BTN.buttonProps { 
          BTN.bsStyle = Defined "danger" ,
          BTN.onClick = Defined $ const $ deleteMachine machineId $ R.navigate (R.companyDetail companyId) router })
        "Smazat"

      extraRows = (case (editing, photos) of
        (Editing, _) -> [photoUploadRow]
        (_, []) -> []
        _ -> [photoCarouselRow]) ++ extraRow ++ (if editing == Editing && null upkeeps && null photos
          then [deleteMachineRow]
          else [])
      setEditing :: InputState -> Fay ()
      setEditing editing' = modify appVar (\appState -> appState {
        D.navigation = case D.navigation appState of
          D.MachineScreen (MD.MachineData a a1 b c c1 c2 v' l l2 l3 (Left (MD.MachineDetail d e _ f g i j))) ->
            D.MachineScreen (MD.MachineData a a1 b c c1 c2 v' l l2 l3 (Left (MD.MachineDetail d e editing' f g i j)))
          _ -> D.navigation appState })
      editButtonRow =
        div' (class' "col-md-3") $
          BTN.button'
            (BTN.buttonProps { BTN.onClick = Defined $ const $ setEditing Editing })
            "Jdi do editačního módu"
      extraFieldsForServer = (\(a,_,b) -> (a,b)) `map` extraFields
      (_, contactPersonId', _) = contactPersonId
      editMachineAction = updateMachine machineId machine otherMachineId contactPersonId'
        extraFieldsForServer (setEditing Display)
      buttonRow'' validationOk = buttonRow' validationOk "Edituj" editMachineAction
      button = case editing of Editing -> buttonRow'' ; _ -> (const editButtonRow)

machineNew :: R.CrmRouter
           -> Var D.AppState
           -> DP.DatePicker
           -> (M.Machine, Text)
           -> MK.MachineKindEnum
           -> C.CompanyId
           -> (MT.MachineType, [US.UpkeepSequence])
           -> Maybe MT.MachineTypeId
           -> (CP.ContactPerson, Maybe CP.ContactPersonId, MD.ContactPersonInMachine)
           -> [(CP.ContactPersonId, CP.ContactPerson)]
           -> V.Validation
           -> Maybe M.MachineId
           -> [(M.MachineId, M.Machine)]
           -> [(EF.ExtraFieldId, MK.MachineKindSpecific, Text)]
           -> DOMElement
machineNew router appState datePickerCalendar (machine', datePickerText) machineSpecific companyId machineTypeTuple 
    machineTypeId (cId @ (contactPerson, contactPersonId, contactPersonActive)) contactPersons v otherMachineId om extraFields = 
  machineDisplay Editing "Nový stroj - fáze 2 - specifické údaje o stroji"
      buttonRow'' appState datePickerCalendar (machine', datePickerText) 
      machineSpecific machineTypeTuple [] Nothing cId contactPersons v otherMachineId om extraFields
    where
      extraFieldsForServer = (\(a,_,b) -> (a,b)) `map` extraFields
      machineTypeEither = case machineTypeId of
        Just(machineTypeId') -> MT.MyInt $ MT.getMachineTypeId machineTypeId'
        Nothing -> MT.MyMachineType machineTypeTuple
      contactPersonId' = case contactPersonActive of
        MD.New  -> Just . M.ContactPersonForMachine $ contactPerson     
        MD.ById -> M.ContactPersonIdForMachine `onJust` contactPersonId
      saveNewMachine = createMachine machine' companyId machineTypeEither contactPersonId' otherMachineId extraFieldsForServer
        (R.navigate (R.companyDetail companyId) router)
      buttonRow'' validationOk = buttonRow' validationOk "Vytvoř" saveNewMachine

machineDisplay :: InputState -- ^ true editing mode false display mode
               -> Text -- ^ header of the page
               -> (ButtonState -> DOMElement)
               -> Var D.AppState
               -> DP.DatePicker
               -> (M.Machine, Text) -- ^ machine, text of the datepicker
               -> MK.MachineKindEnum
               -> (MT.MachineType, [US.UpkeepSequence])
               -> [DOMElement]
               -> Maybe DOMElement
               -> (CP.ContactPerson, Maybe CP.ContactPersonId, MD.ContactPersonInMachine)
               -> [(CP.ContactPersonId, CP.ContactPerson)]
               -> V.Validation
               -> Maybe M.MachineId
               -> [(M.MachineId, M.Machine)]
               -> [(EF.ExtraFieldId, MK.MachineKindSpecific, Text)]
               -> DOMElement
machineDisplay editing pageHeader buttonRow'' appVar operationStartCalendar (machine', datePickerText) 
    _ (machineType, upkeepSequences) extraRows extraGrid (contactPersonId @ (cp, cpId, _))
    contactPersons validation otherMachineId otherMachines extraFields = let

  changeNavigationState :: (MD.MachineData -> MD.MachineData) -> Fay ()
  changeNavigationState fun = modify appVar (\appState -> appState {
    D.navigation = case D.navigation appState of 
      D.MachineScreen md -> D.MachineScreen $ fun md
      _ -> D.navigation appState })

  setMachine :: M.Machine -> Fay ()
  setMachine machine = changeNavigationState 
    (\md -> md { MD.machine = (machine, datePickerText) })
  
  setMachineFull :: (M.Machine, Text) -> Fay ()
  setMachineFull tuple = changeNavigationState
    (\md -> md { MD.machine = tuple })

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
        in setMachineFull (newMachine, text')
    in DP.datePicker editing operationStartCalendar setDatePickerDate 
      setPickerOpenness displayedDate setDate

  validationErrorsGrid = case validation of
    V.Validation [] -> []
    validation' -> [validationHtml $ V.messages validation']
  
  inputRowEditing = inputRow editing
  textareaRowEditing = textareaRow editing
  mkInputRow (efId, efDescription, theValue) = let
    setExtraField string = changeNavigationState $ \machineData -> let
      updateTheExtraField (theSame @ (efId', efDescription', _)) = if efId == efId'
        then (efId', efDescription', string)
        else theSame
      newExtraFields = map updateTheExtraField extraFields
      in machineData { MD.extraFields = newExtraFields }
    in inputRowEditing
      (MK.name efDescription)
      (SetValue theValue)
      setExtraField
  kindSpecificRows = map mkInputRow extraFields

  mkFormGroup = div' (class' "form-group")
  
  noteRow = textareaRowEditing
    "Poznámka"
    (SetValue $ M.note machine') 
    (\str -> setMachine $ machine' { M.note = str })

  -- rows that ore used in the computation by mth
  computationRows = case MT.kind machineType of
    MK.RotaryScrewCompressor -> [
      inputRowEditing
        "Úvodní stav motohodin"
        (DefaultValue $ showInt $ M.initialMileage machine')
        (eventInt' 
          (\im -> let
            newMachine = machine' { M.initialMileage = im }
            newValidation = V.remove V.MachineInitialMileageNumber validation
            in changeNavigationState $ \md -> 
              md { MD.machine = (newMachine, datePickerText) , MD.validation = newValidation })
          (const $ changeNavigationState $ \md -> 
            md { MD.validation = V.add V.MachineInitialMileageNumber validation })) ,
      row 
        "Provoz mth/rok (Rok má 8760 mth)" [
        (div' (class'' ["control-label", "my-text-left", "col-md-3"]) 
          (input 
            editing
            True
            (DefaultValue $ showInt $ M.mileagePerYear machine')
            (let 
              errorHandler = changeNavigationState (\md -> 
                md { MD.validation = V.add V.MachineUsageNumber validation })
              in eventInt' 
                (\mileagePerYear -> if mileagePerYear > 0
                  then changeNavigationState (\md -> md { 
                    MD.validation = V.remove V.MachineUsageNumber validation , 
                    MD.machine = (machine' { M.mileagePerYear = mileagePerYear }, datePickerText)})
                  else errorHandler)
                (const errorHandler)))) ,
        (label' (class'' ["control-label", "col-md-3"]) "Typ provozu") ,
        (let
          upkeepPerMileage = minimum repetitions where
            nonOneTimeSequences = filter (not . US.oneTime) upkeepSequences
            repetitions = map US.repetition nonOneTimeSequences
          operationTypeTuples = [(8760, "24/7"), (upkeepPerMileage, "1 za rok")]
          buttonLabelMaybe = find (\(value, _) -> value == M.mileagePerYear machine') 
            operationTypeTuples
          buttonLabel = maybe "Jiný" snd buttonLabelMaybe
          selectElements = map (\(value, selectLabel) -> let
            selectAction = setMachineFull (machine' { M.mileagePerYear = value }, datePickerText)
            in li $ A.a''' (click selectAction) selectLabel) operationTypeTuples
          buttonLabel' = [text2DOM $ buttonLabel <> " " , span' (class' "caret") ""]
          dropdown' = BD.buttonDropdown' (inputStateToBool editing) buttonLabel' selectElements
          in case editing of
            Editing -> div' (class' "col-md-3") dropdown'
            Display -> div' (class'' ["col-md-3", "control-label", "my-text-left"]) buttonLabel )]]
    _ -> []

  nullDropdownRowEditing = nullDropdownRow editing

  cpPartInputs :: Text
               -> (CP.ContactPerson -> Text)
               -> (CP.ContactPerson -> Text -> CP.ContactPerson) 
               -> [DOMElement]
  cpPartInputs partLabel get set = let
    i = input
      editing
      True
      (SetValue . get $ cp)
      (setCP . set cp)
    in [
      B.col (B.mkColProps 1) partLabel ,
      B.col (B.mkColProps 2) i ]

  setCP :: CP.ContactPerson -> Fay ()
  setCP contactPerson = changeNavigationState $ \md -> let
    (_, a0, a1) = MD.contactPersonId md
    in md { MD.contactPersonId = (contactPerson, a0, a1) }

  elements = div $ [form' (mkAttrs { className = Defined "form-horizontal" }) $
    B.grid $ [
      B.row $ B.col (B.mkColProps 12) $ h2 pageHeader ,
      B.row $ [
        inputRow
          Display
          "Typ zařízení" 
          (SetValue $ MT.machineTypeName machineType) 
          (const $ return ()) ,
        inputRow
          Display
          "Výrobce"
          (SetValue $ MT.machineTypeManufacturer machineType)
          (const $ return ()) ,
        nullDropdownRowEditing 
          "Kontaktní osoba - stávající" 
          contactPersons 
          CP.name 
          (findInList cpId contactPersons) $
          \cpId -> changeNavigationState $ \md -> let
            (a, b, c) = MD.contactPersonId md
            in md { MD.contactPersonId = (a, cpId, c) } ,
        row "Kontaktní osoba - nová" (concat [
          cpPartInputs "Jméno" CP.name (\cp t -> cp { CP.name = t }) ]) ,
        nullDropdownRowEditing 
          "Zapojení" 
          otherMachines 
          M.serialNumber 
          (findInList otherMachineId otherMachines) $
          \omId -> changeNavigationState $ \md -> md { MD.otherMachineId = omId } ,
        inputRowEditing
          "Výrobní číslo"
          (SetValue $ M.serialNumber machine')
          (\s -> setMachine $ machine' { M.serialNumber = s }) ,
        inputRowEditing
          "Rok výroby"
          (SetValue $ M.yearOfManufacture machine')
          (\s -> setMachine $ machine' { M.yearOfManufacture = s }) ,
        editableRow
          editing
          ("Datum uvedení do provozu") 
          datePicker ] ++ computationRows ++ [noteRow] ++ kindSpecificRows ++ extraRows ++ [
          mkFormGroup (buttonRow'' $ (buttonStateFromBool . V.ok) validation) ]]] ++ 
            validationErrorsGrid ++ (case extraGrid of
          Just extraGrid' -> [extraGrid']
          Nothing -> [])
  in elements
