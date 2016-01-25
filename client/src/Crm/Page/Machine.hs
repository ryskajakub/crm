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
import           FFI                                   (Defined(..))

import           HaskellReact                          hiding (row)
import qualified HaskellReact.Bootstrap                as B
import qualified HaskellReact.Bootstrap.Button         as BTN
import qualified HaskellReact.Bootstrap.ButtonDropdown as BD
import qualified HaskellReact.Jasny                    as J
import qualified HaskellReact.Tag.Hyperlink            as A
import qualified HaskellReact.Tag.Image                as IMG
import qualified HaskellReact.Tag.Input                as I
import           HaskellReact.Bootstrap.Carousel       (carousel)
import qualified HaskellReact.BackboneRouter           as BR
import qualified HaskellReact.Bootstrap.Nav            as BN
import qualified HaskellReact.Bootstrap.Glyphicon      as G
import qualified HaskellReact.Bootstrap.Modal          as BM
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
import qualified Crm.Shared.Employee                   as E

import qualified Crm.Data.MachineData                  as MD
import qualified Crm.Data.Data                         as D
import qualified Crm.Component.DatePicker              as DP
import           Crm.Component.Form
import           Crm.Component.Autocomplete
import           Crm.Component.Navigation              as N
import qualified Crm.Component.Photos                  as PH
import           Crm.Server 
import qualified Crm.Runtime                           as Runtime
import           Crm.Helpers 
import qualified Crm.Router                            as R
import qualified Crm.Validation                        as V

machineTypeLabel :: Text
machineTypeLabel = "Typ zařízení" 

machineTypeDisplayRow :: 
  MT.MachineType -> 
  DOMElement
machineTypeDisplayRow machineType =
  inputRow
    Display
    machineTypeLabel
    (SetValue . MT.machineTypeName $ machineType) 
    (const . return $ ())


setEditing :: Var D.AppState -> InputState -> Fay ()
setEditing appVar' editing' = modify appVar' $ \appState -> appState {
  D.navigation = case D.navigation appState of
    D.MachineScreen (MD.MachineData a b c c1 c2 v' l l2 l3 l4 (Left (md @ MD.MachineDetail {}))) ->
      D.MachineScreen (MD.MachineData a b c c1 c2 v' l l2 l3 l4 (Left (md { MD.formState = editing' })))
    _ -> D.navigation appState }


machineDetail :: 
  InputState -> 
  Var D.AppState -> 
  R.CrmRouter -> 
  C.CompanyId -> 
  DP.DatePickerData -> 
  (M.Machine, Text) -> 
  (MT.MachineType, [US.UpkeepSequence]) -> 
  M.MachineId -> 
  Maybe YMD.YearMonthDay -> 
  [(P.PhotoId, PM.PhotoMeta)] -> 
  [(U.UpkeepId, U.Upkeep, UM.UpkeepMachine, [E.Employee], [P.PhotoId])] -> 
  Maybe CP.ContactPersonId -> 
  [(CP.ContactPersonId, CP.ContactPerson)] -> 
  V.Validation -> 
  Maybe M.MachineId -> 
  [(M.MachineId, M.Machine)] -> 
  [(EF.ExtraFieldId, MK.MachineKindSpecific, Text)] -> 
  Maybe MT.MachineTypeId ->
  [(P.PhotoId)] ->
  (DOMElement, Fay ())
machineDetail editing appVar router companyId calendarOpen (machine, 
    usageSetMode) machineTypeTuple machineId nextService photos upkeeps
    contactPersonId contactPersons v otherMachineId om extraFields machineTypeId' upkeepPhotoIds =

  (machineDisplay editing pageHeader button appVar calendarOpen (machine, 
      usageSetMode) machineTypeTuple extraRows extraGrid 
      (contactPersonId, Nothing, Prelude.id) contactPersons v otherMachineId om extraFields
      companyId router machineTypeInputRow, fetchMachinePhotos >> machineTypeCB >> (photoFetch router))
  where
  changeNavigationState :: (MD.MachineData -> MD.MachineData) -> Fay ()
  changeNavigationState fun = modify appVar $ \appState -> appState {
    D.navigation = case D.navigation appState of 
      D.MachineScreen md -> D.MachineScreen $ fun md
      _ -> D.navigation appState }
  pageHeader = case editing of Editing -> "Editace stroje"; _ -> "Stroj"
  extraRow = maybe [] (\nextService' -> [editableRow Display "Další servis" (displayDate nextService')]) nextService

  setPhotosInModal :: [P.PhotoId] -> Fay ()
  setPhotosInModal photoIds = changeNavigationState $ \md -> case MD.machinePageMode md of 
    (Left (mdt @ MD.MachineDetail {})) -> md { MD.machinePageMode = Left (mdt { MD.upkeepPhotoIds = photoIds }) }
    _ -> md

  BM.ModalPair modalButtonProps modalElementBase = BM.mkModalPair 
  modalElement = modalElementBase . div' (class' "upkeep-photos") . map mkPhotoRegion $ upkeepPhotoIds
    where
    mkPhotoRegion photoId = IMG.image' 
      (mkAttrs { id = Defined . (<>) "photo-" . showInt . P.getPhotoId $ photoId})
      (IMG.mkImageAttrs "")

  PH.PhotoModal photoModalElement mkPhotoModalButton photoFetch = PH.mkPhotoModal upkeepPhotoIds

  upkeepHistoryHtml = let
    mkUpkeepRow :: (U.UpkeepId, U.Upkeep, UM.UpkeepMachine, [E.Employee], [P.PhotoId]) -> DOMElement
    mkUpkeepRow (upkeepId, upkeep, upkeepMachine, employees, photoIds) = let
      mth = showInt . UM.recordedMileage $ upkeepMachine
      warranty = if UM.warrantyUpkeep upkeepMachine then span' (class'' ["label", "label-danger"]) "Z" else text2DOM ""
      repair = let
        (labelClass, labelText) = if UM.repair upkeepMachine 
          then ("label-danger", "O")
          else ("label-info", "S")
        in span' (class'' [labelClass, "label"]) labelText
      employeeCol = mkColours employees
      action = if U.upkeepClosed upkeep
        then let
          doReopen = reopenUpkeep upkeepId goEditUpkeep router
          goEditUpkeep = R.navigate (R.upkeepDetail upkeepId) router
          buttonProps = BTN.buttonProps {
            BTN.bsStyle = Defined "primary" ,
            BTN.onClick = Defined . const $ doReopen }
          in BTN.button' buttonProps "Otevřít"
        else text2DOM ""
      displayPhotos = mkPhotoModalButton photoIds setPhotosInModal
      in tr [th . displayDate . U.upkeepDate $ upkeep, td warranty, td repair, td mth, td employeeCol, 
        td . U.workDescription $ upkeep, td . U.recommendation $ upkeep, td . UM.upkeepMachineNote $ 
        upkeepMachine, td . UM.endNote $ upkeepMachine, td action, td displayPhotos]
    rows = map mkUpkeepRow upkeeps
    mkTable bodyRows = B.table [
      colgroup $ col' (class' "nowrap") "" : map (const . col $ "") [(1::Int)..9] ,
      thead . tr $ [th "Datum", th G.thumbsUp, th G.flash , th "Mth", th G.user, th "Popis práce", 
        th "Doporučení", th "Poznámka", th "Koncová poznámka", th "Akce", th "Listy"] , 
      tbody' (class' "past-services") bodyRows]
    in if null rows
      then []
      else (B.fullRow . h3 $ "Předchozí servisy") : (B.fullRow . mkTable $ rows) : []
  extraGrid = (if editing == Editing && (not $ null upkeeps) 
    then Nothing 
    else (Just $ div [ B.grid upkeepHistoryHtml , photoModalElement ]))
  
  editableRowEditing = editableRow editing

  photoUploadRow = editableRowEditing
    "Fotka (Max. 2MB)" 
    (let 
      imageUploadHandler = const $ do
        fileUpload <- JQ.select "#file-upload"
        files <- getFileList fileUpload
        file <- fileListElem 0 files
        type' <- fileType file
        name <- fileName file
        uploadMachinePhotoData machineId file $ \photoId ->
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
  extraFieldsForServer = (\(a,_,b) -> (a,b)) `map` extraFields
  editMachineAction = case machineTypeId' of
    Just (machineTypeId) -> updateMachine machineId machineTypeId machine otherMachineId 
      contactPersonId extraFieldsForServer (setEditing appVar Display) router
    Nothing -> return ()
  buttonRow'' validationOk = buttonRow' validationOk "Edituj" editMachineAction
  button = case editing of Editing -> Just buttonRow'' ; _ -> Nothing

  fetchMachinePhotos = PH.fetchPhotos (map fst photos) router
  fetchUpkeepPhotos = PH.fetchPhotos upkeepPhotoIds router

  (machineTypeInputRow, machineTypeCB) = let
    (machineTypeAutocomplete, cb) = 
      autocompleteInput
        inputNormalAttrs
        (const $ changeNavigationState $ \md -> let
          emptyMachineType = (fst machineTypeTuple) {
            MT.machineTypeManufacturer = "???" }
          in md {
            MD.validation = V.add V.MachineTypeNotSelected (MD.validation md) ,
            MD.machineTypeTuple = (emptyMachineType, snd machineTypeTuple) ,
            MD.machineTypeId = Nothing })
        (\text -> if text /= "" 
          then fetchMachineType text (\maybeTuple -> case maybeTuple of
            Just (machineTypeId', machineType', _, sequences) -> do
              changeNavigationState $ \md -> md {
                MD.validation = V.remove V.MachineTypeNotSelected (MD.validation md) ,
                MD.machineTypeTuple = (machineType', sequences) ,
                MD.machineTypeId = Just machineTypeId' }
            Nothing -> return ()) router
          else return ())
        (\a b -> fetchMachineTypesAutocomplete a b router)
        "machine-type-edit-autocomplete"
        (I.mkInputAttrs { I.defaultValue = Defined . MT.machineTypeName . fst $ machineTypeTuple })
    changeInputRow = oneElementRow
      machineTypeLabel
      machineTypeAutocomplete
    in case editing of
      Editing -> (changeInputRow, cb)
      Display -> (machineTypeDisplayRow . fst $ machineTypeTuple, return ())


machineNew :: 
  R.CrmRouter -> 
  Var D.AppState -> 
  DP.DatePickerData -> 
  (M.Machine, Text) -> 
  C.CompanyId -> 
  (MT.MachineType, [US.UpkeepSequence]) -> 
  Maybe MT.MachineTypeId -> 
  (CP.ContactPerson, Maybe CP.ContactPersonId, MD.ContactPersonInMachine) -> 
  [(CP.ContactPersonId, CP.ContactPerson)] -> 
  V.Validation -> 
  Maybe M.MachineId -> 
  [(M.MachineId, M.Machine)] -> 
  [(EF.ExtraFieldId, MK.MachineKindSpecific, Text)] -> 
  DOMElement
machineNew router appVar datePickerCalendar (machine', usageSetMode) companyId machineTypeTuple machineTypeId 
    (contactPerson, contactPersonId, contactPersonActiveRow) contactPersons v otherMachineId om extraFields = 
  machineDisplay Editing "Nový stroj - fáze 2 - specifické údaje o stroji" (Just buttonRow'') appVar 
    datePickerCalendar (machine', usageSetMode) machineTypeTuple [] Nothing (contactPersonId, 
    Just (newContactPersonRow, setById), byIdHighlight) contactPersons v otherMachineId om extraFields
    companyId router (machineTypeDisplayRow (fst machineTypeTuple))
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


machineDisplay :: 
  InputState -> 
  Text -> -- ^ header of the page
  Maybe (ButtonState -> DOMElement) ->
  Var D.AppState ->
  DP.DatePickerData ->
  (M.Machine, Text) -> -- ^ machine, text of the datepicker
  (MT.MachineType, [US.UpkeepSequence]) ->
  [DOMElement] ->
  Maybe DOMElement ->
  (Maybe CP.ContactPersonId, Maybe (DOMElement, Fay ()), DOMElement -> DOMElement) ->
  [(CP.ContactPersonId, CP.ContactPerson)] ->
  V.Validation ->
  Maybe M.MachineId ->
  [(M.MachineId, M.Machine)] ->
  [(EF.ExtraFieldId, MK.MachineKindSpecific, Text)] ->
  C.CompanyId ->
  R.CrmRouter ->
  DOMElement ->
  DOMElement
machineDisplay editing pageHeader buttonRow'' appVar operationStartCalendarDpd (machine', rawUsage) (machineType, 
    upkeepSequences) extraRows extraGrid (dropdownContactPersonId, newContactPersonRow, dropdownCPHighlight) contactPersons 
    validation otherMachineId otherMachines extraFields companyId router machineTypeRow = mkGrid where

  changeNavigationState :: (MD.MachineData -> MD.MachineData) -> Fay ()
  changeNavigationState fun = modify appVar (\appState -> appState {
    D.navigation = case D.navigation appState of 
      D.MachineScreen md -> D.MachineScreen $ fun md
      _ -> D.navigation appState })

  setMachine :: M.Machine -> Fay ()
  setMachine machine = setMachineFull machine
  
  setMachineFull :: M.Machine -> Fay ()
  setMachineFull machine = changeNavigationState
    (\md -> md { MD.machine = (machine, rawUsage)})

  datePicker = let
    setDpd dpd' = changeNavigationState $ \md -> md { MD.operationStartCalendar = dpd' }
    setMachineOpStart d = setMachine $ machine' { M.machineOperationStartDate = Just d }
    machineOpStart = maybe YMD.new (\x -> x) (M.machineOperationStartDate machine')
    in DP.datePicker' editing operationStartCalendarDpd setDpd machineOpStart setMachineOpStart

  validationErrorsSection = case validation of
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
    "Další specifikace"
    (SetValue . M.note $ machine')
    (\str -> setMachine $ machine' { M.note = str })
  labelRow = inputRowEditing
    "Označení"
    (SetValue $ M.label_ machine') 
    (\str -> setMachine $ machine' { M.label_ = str })
  archivedRow = editableRow editing "Archivován" archivedCheckbox where
    archivedCheckbox = div' (if editing == Editing then class' "editing-checkbox" else mkAttrs) $
      checkbox editing (M.archived machine') (\archived -> setMachine $ machine' { M.archived = archived })

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
            MD.machine = (newMachine, showInt . M.mileagePerYear $ newMachine) ,
            MD.validation = newValidation })
        (\t -> changeNavigationState $ \md -> md { 
          MD.validation = V.add V.MachineInitialMileageNumber validation , 
          MD.machine = (machine', t) }))]
  usageRows = [
    row 
      "Provoz mth/rok (Rok má 8760 mth)" [
      (div' (class'' ["control-label", "my-text-left", "col-md-3"]) 
        (input 
          editing
          True
          (SetValue rawUsage)
          (let 
            errorHandler t = changeNavigationState $ \md -> md { 
              MD.machine = (machine', t) ,
              MD.validation = V.add V.MachineUsageNumber validation }
            in eventInt' 
              (> 0)
              (\mileagePerYear ->
                changeNavigationState $ \md -> md { 
                  MD.validation = V.remove V.MachineUsageNumber validation , 
                  MD.machine = (machine' { M.mileagePerYear = mileagePerYear }, showInt mileagePerYear)})
              errorHandler))) ,
      (label' (class'' ["control-label", "col-md-3"]) "Typ provozu") ,
      (let
        upkeepPerMileage = minimum repetitions where
          nonOneTimeSequences = filter (not . US.oneTime) upkeepSequences
          repetitions = map US.repetition nonOneTimeSequences
        preselectedOperationTypes = 
          (8760, "24/7") :
          (if upkeepPerMileage * 4 <= 8760 then [(upkeepPerMileage * 4, "1x za čtvrt roku")] else []) ++
          (if upkeepPerMileage * 2 <= 8760 then [(upkeepPerMileage * 2, "1x za půl roku")] else []) ++ [
          (upkeepPerMileage, "1x za rok") ,
          (truncate $ fromIntegral upkeepPerMileage / (2 :: Double), "1x za 2 roky") ,
          (truncate $ fromIntegral upkeepPerMileage / (3 :: Double), "1x za 3 roky") ,
          (truncate $ fromIntegral upkeepPerMileage / (4 :: Double), "1x za 4 roky") ]
        buttonLabelMaybe = find (\(value, _) -> value == M.mileagePerYear machine') 
          preselectedOperationTypes
        selectAction Nothing = return ()
        selectAction (Just value) = changeNavigationState $ \md -> md {
          MD.machine = (machine' { M.mileagePerYear = value }, showInt value) }
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
    MK.RotaryScrewCompressor -> rotaryScrewCompressorInputs ++ usageRows
    MK.VacuumPump -> usageRows
    _ -> []

  formInputs = [
    inputRow
      Display
      "Druh zařízení"
      (SetValue . MK.kindToStringRepr . MT.kind $ machineType)
      (const . return $ ()) ,
    machineTypeRow ,
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
      datePicker ] ++ computationRows ++ [labelRow, archivedRow, noteRow] ++ kindSpecificRows ++ extraRows ++ 
        maybe [] (\mkButtonRow -> [
          mkFormGroup (mkButtonRow $ (buttonStateFromBool . V.ok) validation)]) buttonRow''
 
  mkGrid = let
    headerSection = let
      button = BTN.button'
        BTN.buttonProps { BTN.onClick = Defined . const . setEditing appVar $ Editing }
        "Jdi do editačního módu"
      goEditButton = case editing of
        Editing -> []
        _ -> [form' (class' "navbar-form") button]
      in B.grid [
        B.row $ B.col (B.mkColProps 12) $ h2 pageHeader ,
        B.fullRow $ BN.nav $ N.backToCompany companyId router : goEditButton]
    formSection = form' (mkAttrs { className = Defined "form-horizontal" }) $
      B.grid . B.row $ formInputs
    extraSection = 
      case extraGrid of
        Just extraGrid' -> [extraGrid']
        Nothing -> []
    in div $ headerSection : formSection : validationErrorsSection ++ extraSection
