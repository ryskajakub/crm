{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Crm.Page.Machine (
  machineNew ,
  machineDetail ) where

import           Data.Text                             (fromString, (<>), Text, showInt, pack, putStrLn)
import           Prelude                               hiding (div, span, id, putStrLn)
import qualified Prelude                 
import           Data.Var                              (Var, modify)
import           Data.Maybe                            (onJust, maybeToList)
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
import qualified Crm.Shared.Api                        as A

import qualified Crm.Data.MachineData                  as MD
import qualified Crm.Data.Data                         as D
import qualified Crm.Component.DatePicker              as DP
import           Crm.Component.Form
import           Crm.Server 
import qualified Crm.Runtime                           as Runtime
import           Crm.Helpers 
import qualified Crm.Router                            as R
import qualified Crm.Validation                        as V


machineDetail :: InputState
              -> Var D.AppState
              -> R.CrmRouter
              -> C.CompanyId
              -> DP.DatePicker
              -> (M.Machine, Text, Text)
              -> MK.MachineKindEnum
              -> (MT.MachineType, [US.UpkeepSequence])
              -> M.MachineId
              -> YMD.YearMonthDay
              -> [(P.PhotoId, PM.PhotoMeta)]
              -> [(U.UpkeepId, U.Upkeep, UM.UpkeepMachine)]
              -> Maybe CP.ContactPersonId
              -> [(CP.ContactPersonId, CP.ContactPerson)]
              -> V.Validation
              -> Maybe M.MachineId
              -> [(M.MachineId, M.Machine)]
              -> [(EF.ExtraFieldId, MK.MachineKindSpecific, Text)]
              -> (DOMElement, Fay ())
machineDetail editing appVar router companyId calendarOpen (machine, 
    datePickerText, usageSetMode) machineSpecific machineTypeTuple machineId nextService photos upkeeps
    contactPersonId contactPersons v otherMachineId om extraFields =

  (machineDisplay editing pageHeader button appVar calendarOpen (machine, 
      datePickerText, usageSetMode) machineSpecific machineTypeTuple extraRows extraGrid 
      (contactPersonId, Nothing, Prelude.id) contactPersons v otherMachineId om extraFields, fetchPhotos)
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
          uploadPhotoMeta (PM.PhotoMeta type' name) photoId BR.refresh router
      imageUploadLabel = "Přidej fotku"
      photoList = map (\(photoId, photoMeta) -> let
        deletePhotoHandler = const $ deletePhoto photoId BR.refresh router
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
  mkPhoto (photoId, _) = IMG.image' 
    (mkAttrs { id = Defined . (<>) "photo-" . showInt . P.getPhotoId $ photoId} ) 
    (IMG.mkImageAttrs "")
  photoCarouselRow = editableRowEditing
    "Fotky"
    (carousel "my-carousel" (map mkPhoto photos))

  deleteMachineRow = oneElementRow "Smazat" $ BTN.button'
    (BTN.buttonProps { 
      BTN.bsStyle = Defined "danger" ,
      BTN.onClick = Defined $ const $ deleteMachine machineId (R.navigate (R.companyDetail companyId) router) router })
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
  editMachineAction = updateMachine machineId machine otherMachineId contactPersonId
    extraFieldsForServer (setEditing Display) router
  buttonRow'' validationOk = buttonRow' validationOk "Edituj" editMachineAction
  button = case editing of Editing -> buttonRow'' ; _ -> (const editButtonRow)

  fetchPhotos = forM_ photos $ \(photoId, _) -> Runtime.passwordAjax
    (pack A.photos <> "/" <> (showInt . P.getPhotoId $ photoId))
    (\imageData -> do
      putStrLn imageData
      let photoHTMLId = ((<>) "#photo-" . showInt . P.getPhotoId $ photoId)
      photoHtmlElement <- JQ.select photoHTMLId
      _ <- JQ.setAttr "src" ("data:image/jpeg;base64," <> imageData) photoHtmlElement 
      return ())
    Nothing
    Runtime.get
    Nothing
    Nothing
    router


machineNew :: R.CrmRouter
           -> Var D.AppState
           -> DP.DatePicker
           -> (M.Machine, Text, Text)
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
machineNew router appVar datePickerCalendar (machine', datePickerText, usageSetMode) machineSpecific companyId machineTypeTuple 
    machineTypeId (contactPerson, contactPersonId, contactPersonActiveRow) contactPersons v otherMachineId om extraFields = 
  machineDisplay Editing "Nový stroj - fáze 2 - specifické údaje o stroji"
      buttonRow'' appVar datePickerCalendar (machine', datePickerText, usageSetMode) machineSpecific machineTypeTuple 
      [] Nothing (contactPersonId, Just (newContactPersonRow, setById), byIdHighlight) contactPersons v otherMachineId om extraFields
  where
  extraFieldsForServer = (\(a,_,b) -> (a,b)) `map` extraFields
  machineTypeEither = case machineTypeId of
    Just(machineTypeId') -> MT.MyInt $ MT.getMachineTypeId machineTypeId'
    Nothing -> MT.MyMachineType machineTypeTuple
  contactPersonId' = case contactPersonActiveRow of
    MD.New  -> Just . M.ContactPersonForMachine $ contactPerson     
    MD.ById -> M.ContactPersonIdForMachine `onJust` contactPersonId
  saveNewMachine = createMachine machine' companyId machineTypeEither contactPersonId' otherMachineId extraFieldsForServer
    (R.navigate (R.companyDetail companyId) router) router
  buttonRow'' validationOk = buttonRow' validationOk "Vytvoř" saveNewMachine

  cpPartInputs :: Text
               -> (CP.ContactPerson -> Text)
               -> (CP.ContactPerson -> Text -> CP.ContactPerson) 
               -> [DOMElement]
  cpPartInputs partLabel get set = let
    i = input
      Editing
      True
      (SetValue . get $ contactPerson)
      (setCP . set contactPerson)
    in [
      div' (class'' ["control-label", "my-text-left", "col-md-1"]) partLabel ,
      B.col (B.mkColProps 2) i ]

  setCP :: CP.ContactPerson -> Fay ()
  setCP contactPerson' = changeNavigationState $ \mn ->
    mn { MD.contactPersonNew = (contactPerson', MD.New) }

  (byIdHighlight, newHighlight) = let
    f = case contactPersonActiveRow of
      MD.ById -> Prelude.id
      MD.New  -> swap
    in f (span' (class' "underline"), Prelude.id)

  newContactPersonRow = row (newHighlight . text2DOM $ "Kontaktní osoba - nová") $ concat [
    cpPartInputs "Jméno" CP.name $ \cp t -> cp { CP.name = t } ,
    cpPartInputs "Telefon" CP.phone $ \cp t -> cp { CP.phone = t } ,
    cpPartInputs "Pozice" CP.position $ \cp t -> cp { CP.position = t } ]

  setById :: Fay ()
  setById = changeNavigationState $ \mn -> mn { MD.contactPersonNew = (contactPerson, MD.ById) }

  changeNavigationState :: (MD.MachineNew -> MD.MachineNew) -> Fay ()
  changeNavigationState f = modify appVar $ \appState -> appState {
    D.navigation = case D.navigation appState of 
      D.MachineScreen (md @ (MD.MachineData _ _ _ _ _ _ _ _ _ _ (Right (mn @ MD.MachineNew {})))) -> 
        D.MachineScreen (md { MD.machinePageMode = Right . f $ mn })
      _ -> D.navigation appState }


machineDisplay :: InputState -- ^ true editing mode false display mode
               -> Text -- ^ header of the page
               -> (ButtonState -> DOMElement)
               -> Var D.AppState
               -> DP.DatePicker
               -> (M.Machine, Text, Text) -- ^ machine, text of the datepicker
               -> MK.MachineKindEnum
               -> (MT.MachineType, [US.UpkeepSequence])
               -> [DOMElement]
               -> Maybe DOMElement
               -> (Maybe CP.ContactPersonId, Maybe (DOMElement, Fay ()), DOMElement -> DOMElement)
               -> [(CP.ContactPersonId, CP.ContactPerson)]
               -> V.Validation
               -> Maybe M.MachineId
               -> [(M.MachineId, M.Machine)]
               -> [(EF.ExtraFieldId, MK.MachineKindSpecific, Text)]
               -> DOMElement
machineDisplay editing pageHeader buttonRow'' appVar operationStartCalendar (machine', datePickerText, rawUsage)
    _ (machineType, upkeepSequences) extraRows extraGrid (dropdownContactPersonId, newContactPersonRow, dropdownCPHighlight)
    contactPersons validation otherMachineId otherMachines extraFields = mkGrid where

  changeNavigationState :: (MD.MachineData -> MD.MachineData) -> Fay ()
  changeNavigationState fun = modify appVar (\appState -> appState {
    D.navigation = case D.navigation appState of 
      D.MachineScreen md -> D.MachineScreen $ fun md
      _ -> D.navigation appState })

  setMachine :: M.Machine -> Fay ()
  setMachine machine = setMachineFull (machine, datePickerText)
  
  setMachineFull :: (M.Machine, Text) -> Fay ()
  setMachineFull (machine, datePickerText') = changeNavigationState
    (\md -> md { MD.machine = (machine, datePickerText', rawUsage) })

  datePicker = let
    setDatePickerDate date = changeNavigationState $ \state ->
      state { MD.operationStartCalendar = 
        lmap (const date) (MD.operationStartCalendar state )}
    setPickerOpenness openness = changeNavigationState $ \state ->
      state { MD.operationStartCalendar = 
        rmap (const openness) (MD.operationStartCalendar state )}
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
  
  noteRow = inputRowEditing
    "Označení"
    (SetValue $ M.label machine') 
    (\str -> setMachine $ machine' { M.label = str })

  rotaryScrewCompressorInputs = [
    inputRowEditing
      "Úvodní stav motohodin"
      (DefaultValue . showInt $ M.initialMileage machine')
      (eventInt' 
        (const True)
        (\im -> let
          newMachine = machine' { M.initialMileage = im }
          newValidation = V.remove V.MachineInitialMileageNumber validation
          in changeNavigationState $ \md -> md { 
            MD.machine = (newMachine, datePickerText, showInt . M.mileagePerYear $ newMachine) ,
            MD.validation = newValidation })
        (\t -> changeNavigationState $ \md -> md { 
          MD.validation = V.add V.MachineInitialMileageNumber validation , 
          MD.machine = (machine', datePickerText, t) })) ,
    row 
      "Provoz mth/rok (Rok má 8760 mth)" [
      (div' (class'' ["control-label", "my-text-left", "col-md-3"]) 
        (input 
          editing
          True
          (SetValue rawUsage)
          (let 
            errorHandler t = changeNavigationState $ \md -> md { 
              MD.machine = (machine', datePickerText, t) ,
              MD.validation = V.add V.MachineUsageNumber validation }
            in eventInt' 
              (> 0)
              (\mileagePerYear ->
                changeNavigationState $ \md -> md { 
                  MD.validation = V.remove V.MachineUsageNumber validation , 
                  MD.machine = (machine' { M.mileagePerYear = mileagePerYear }, datePickerText, showInt mileagePerYear)})
              errorHandler))) ,
      (label' (class'' ["control-label", "col-md-3"]) "Typ provozu") ,
      (let
        upkeepPerMileage = minimum repetitions where
          nonOneTimeSequences = filter (not . US.oneTime) upkeepSequences
          repetitions = map US.repetition nonOneTimeSequences
        preselectedOperationTypes = [
          (8760, "24/7") ,
          (upkeepPerMileage, "1x za rok") , 
          (truncate $ fromIntegral upkeepPerMileage / (2 :: Double), "1x za 2 roky") ] ++
          (if upkeepPerMileage * 2 <= 8760 then [(upkeepPerMileage * 2, "1x za půl roku")] else [])
        buttonLabelMaybe = find (\(value, _) -> value == M.mileagePerYear machine') 
          preselectedOperationTypes
        selectAction Nothing = return ()
        selectAction (Just value) = changeNavigationState $ \md -> md {
          MD.machine = (machine' { M.mileagePerYear = value }, datePickerText, showInt value) }
        (operationTypesDropdown, buttonLabel) = nullDropdown 
          preselectedOperationTypes
          text2DOM
          (fst `onJust` buttonLabelMaybe)
          selectAction
        dd = case editing of
          Editing -> div' (class' "col-md-3") operationTypesDropdown
          Display -> div' (class'' ["col-md-3", "control-label", "my-text-left"]) buttonLabel 
        in dd )]]


  -- rows that ore used in the computation by mth
  computationRows = case MT.kind machineType of
    MK.RotaryScrewCompressor -> rotaryScrewCompressorInputs
    _ -> []

  formInputs = [
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
    nullDropdownRow
      editing
      (dropdownCPHighlight . text2DOM $ "Kontaktní osoba - stávající")
      contactPersons 
      (text2DOM . CP.name)
      dropdownContactPersonId $
      \cpId -> do
        changeNavigationState $ \md -> md { MD.contactPersonId = cpId }
        case newContactPersonRow of
          Just (_, setById) -> setById
          _                 -> return () ] ++
    maybeToList (fst `onJust` newContactPersonRow) ++ [
    nullDropdownRow
      editing 
      "Zapojení" 
      otherMachines 
      (text2DOM . M.serialNumber)
      otherMachineId $
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
      mkFormGroup (buttonRow'' $ (buttonStateFromBool . V.ok) validation) ]

  mkGrid = 
    div $ [
      (form' (mkAttrs { className = Defined "form-horizontal" }) $
        B.grid $ [
          (B.row $ B.col (B.mkColProps 12) $ h2 pageHeader),
          B.row formInputs]) :
      validationErrorsGrid ++ 
      (case extraGrid of
        Just extraGrid' -> [extraGrid']
        Nothing -> [])]
