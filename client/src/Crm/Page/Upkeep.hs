{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Crm.Page.Upkeep (
  upkeepNew ,
  upkeepDetail ,
  plannedUpkeeps ) where

import           Data.Text                        (fromString, Text, showInt, (<>))
import           Prelude                          hiding (div, span, id)
import qualified Prelude                          as Prelude
import           Data.Var (Var, modify)
import           Data.Maybe                       (mapMaybe)
import           FFI (Defined(Defined))

import           HaskellReact                     as HR
import qualified HaskellReact.Bootstrap           as B
import qualified HaskellReact.Bootstrap.Button    as BTN
import qualified HaskellReact.Bootstrap.Glyphicon as G
import qualified HaskellReact.Tag.Hyperlink       as A

import qualified Crm.Shared.Company               as C
import qualified Crm.Shared.Machine               as M
import qualified Crm.Shared.MachineType           as MT
import qualified Crm.Shared.Upkeep                as U
import qualified Crm.Shared.Employee              as E
import qualified Crm.Shared.UpkeepMachine         as UM
import qualified Crm.Shared.UpkeepSequence        as US

import qualified Crm.Data.Data                    as D
import qualified Crm.Data.UpkeepData              as UD
import qualified Crm.Component.DatePicker         as DP
import qualified Crm.Validation                   as V
import qualified Crm.Router                       as R
import           Crm.Server                       (createUpkeep, updateUpkeep)
import           Crm.Component.Form
import           Crm.Helpers
import           Crm.Types                        (DisplayedNote (..))

plannedUpkeeps :: R.CrmRouter
               -> [(U.UpkeepId, U.Upkeep, C.CompanyId, C.Company, Text)]
               -> DOMElement
plannedUpkeeps router upkeepCompanies = let
  head' = thead $ tr [
    th "Název firmy" ,
    th "Adresa" ,
    th "Poznámky" ,
    th "Datum" ,
    th "Přeplánovat" ,
    th "Uzavřít" ]
  body = tbody $ map (\(upkeepId, upkeep, companyId, company, notes) ->
    tr [
      td $ R.link
        (C.companyName company)
        (R.companyDetail companyId)
        router ,
      td $ C.companyAddress company ,
      td $ notes ,
      let 
        date = U.upkeepDate upkeep
        in td $ R.link 
          (displayDate date)
          (R.dailyPlan date Nothing)
          router ,
      td $ R.link
        "Přeplánovat"
        (R.replanUpkeep upkeepId)
        router,
      td $ R.link
        "Uzavřít"
        (R.upkeepDetail upkeepId)
        router ]) upkeepCompanies

  advice = p [ text2DOM "Seznam naplánovaných servisů. Tady můžeš buď servis ", strong "přeplánovat", text2DOM ", pokud je třeba u naplánovaného změnit datum a podobně, nebo můžeš servis uzavřít, to se dělá potom co je servis fyzicky hotov a přijde ti servisní list." ]
  pageInfo' = pageInfo "Naplánované servisy" $ Just advice

  in B.grid $ B.row $
    pageInfo' ++
    [B.col (B.mkColProps 12) $ main $ B.table [head', body]]


-- | if the element is in the first list, put it in the other one, if the element
-- is in the other, put in the first list
toggle :: ([a],[a]) -> (a -> Bool) -> ([a],[a])
toggle lists findElem = let
  toggleInternal (list1, list2) runMore = let
    foundInFirstList = find findElem list1
    in case foundInFirstList of
      Just(elem') -> let
        filteredList1 = filter (not . findElem) list1
        addedToList2 = elem' : list2
        result = (filteredList1, addedToList2)
        in if runMore then result else swap result
      _ -> if runMore
        then toggleInternal (list2, list1) False
        else lists
  in toggleInternal lists True


mkSubmitButton :: Renderable a
               => a
               -> Fay ()
               -> Bool
               -> DOMElement
mkSubmitButton buttonLabel handler enabled = let
  basicButtonProps = BTN.buttonProps {
    BTN.bsStyle = Defined "primary" }
  buttonProps = if enabled
    then basicButtonProps { BTN.onClick = Defined $ const handler }
    else basicButtonProps { BTN.disabled = Defined True }
  in BTN.button' buttonProps buttonLabel


upkeepDetail :: R.CrmRouter
             -> Var D.AppState
             -> U.Upkeep'
             -> (DP.DatePicker, Text)
             -> [UM.UpkeepMachine']
             -> [(M.MachineId, M.Machine, MT.MachineType, US.UpkeepSequence)] 
             -> C.CompanyId -- ^ company id
             -> [E.Employee']
             -> [Maybe E.EmployeeId]
             -> V.Validation
             -> DisplayedNote
             -> DOMElement
upkeepDetail router appState upkeep3 datePicker notCheckedMachines 
    machines companyId employees selectedEmployees v dnf =
  upkeepForm appState "Uzavřít servis" upkeep2 datePicker notCheckedMachines 
    machines submitButton True employees selectedEmployees v dnf
      where
        (_,upkeep,upkeepMachines) = upkeep3
        upkeep2 = (upkeep,upkeepMachines)
        submitButton = let
          closeUpkeepHandler = updateUpkeep
            upkeep3
            (mapMaybe Prelude.id selectedEmployees)
            (R.navigate (R.maintenances companyId) router)
          in mkSubmitButton 
            [span G.plus , span " Uzavřít"]
            closeUpkeepHandler


upkeepNew :: R.CrmRouter
          -> Var D.AppState
          -> (U.Upkeep, [UM.UpkeepMachine'])
          -> (DP.DatePicker, Text)
          -> [UM.UpkeepMachine']
          -> [(M.MachineId, M.Machine, MT.MachineType, US.UpkeepSequence)] -- ^ machine ids -> machines
          -> Either C.CompanyId U.UpkeepId
          -> [E.Employee']
          -> [Maybe E.EmployeeId]
          -> V.Validation
          -> DOMElement
upkeepNew router appState upkeep datePicker notCheckedMachines machines upkeepIdentification es se v = 
  upkeepForm appState pageHeader upkeep datePicker notCheckedMachines machines submitButton False es se v NoChoice where
    (upkeepU, upkeepMachines) = upkeep
    (pageHeader, submitButton) = case upkeepIdentification of 
      Left _ -> let
        newUpkeepHandler = createUpkeep
          (upkeepU, upkeepMachines, mapMaybe Prelude.id se)
          (R.navigate R.plannedUpkeeps router)
        button = mkSubmitButton
          [G.plus , text2DOM " Naplánovat"]
          newUpkeepHandler
        in ("Naplánovat servis", button)
      Right (upkeepId) -> let
        replanUpkeepHandler = updateUpkeep
          (upkeepId, upkeepU, upkeepMachines)
          (mapMaybe Prelude.id se)
          (R.navigate R.plannedUpkeeps router)
        button = mkSubmitButton
          [text2DOM "Přeplánovat"]
          replanUpkeepHandler
        in ("Přeplánovat servis", button)


mapEither :: (a -> a') -> Either a b -> Either a' b
mapEither f (Left a) = Left . f $ a
mapEither _ (Right b) = Right b


upkeepForm :: Var D.AppState
           -> Text -- ^ page header
           -> (U.Upkeep, [(UM.UpkeepMachine')])
           -> (DP.DatePicker, Text) -- ^ datepicker, datepicker openness
           -> [(UM.UpkeepMachine')]
           -> [(M.MachineId, M.Machine, MT.MachineType, US.UpkeepSequence)] 
              -- ^ machine ids -> machines
           -> (Bool -> DOMElement) -- ^ submit button
           -> Bool -- ^ display the mth input field
           -> [E.Employee']
           -> [Maybe E.EmployeeId]
           -> V.Validation
           -> DisplayedNote
           -> DOMElement
upkeepForm appState pageHeader (upkeep, upkeepMachines) (upkeepDatePicker', rawUpkeepDate)
    uncheckedMachines machines button closeUpkeep' employees selectedEmployees validation displayedNoteFlag = let
  upkeepFormRows = 
    [companyNameHeader] ++
    [formHeader] ++
    map upkeepMachineRow machines ++ 
    [dateRow] ++ 
    employeeSelectRows ++
    closeUpkeepRows ++ 
    [submitButtonRow]

  modify' :: (UD.UpkeepData -> UD.UpkeepData) -> Fay ()
  modify' fun = modify appState $ \appState' -> let
    newState = case D.navigation appState' of
      D.UpkeepScreen ud -> D.UpkeepScreen $ fun ud
    in appState' { D.navigation = newState }

  setUpkeep :: (U.Upkeep, [UM.UpkeepMachine']) -> Text -> Fay ()
  setUpkeep modifiedUpkeep upkeepDateText = modify' $ \upkeepData ->
    let dp = fst $ UD.upkeepDatePicker upkeepData
    in upkeepData { 
      UD.upkeep = modifiedUpkeep ,
      UD.upkeepDatePicker = (dp, upkeepDateText) }
  
  setNotCheckedMachines :: [UM.UpkeepMachine'] -> [UM.UpkeepMachine'] -> Fay ()
  setNotCheckedMachines checkedMachines notCheckedMachines' = modify' $ 
    \upkeepData -> upkeepData { 
      UD.upkeep = (upkeep, checkedMachines) ,
      UD.notCheckedMachines = notCheckedMachines' }

  textareaRowEditing = textareaRow Editing
  inputRowEditing = inputRow Editing

  machineColsSize = 4
  (closeUpkeepRows, noteColsSize) = if closeUpkeep' 
    then ([inputRowEditing "Hodiny"
        (SetValue $ U.workHours upkeep) $ \es -> modify' $ \ud ->
          ud { UD.upkeep = lmap (const $ upkeep { U.workHours = es }) (UD.upkeep ud) } ,
      textareaRowEditing "Popis práce" (SetValue $ U.workDescription upkeep) $ 
        \es -> modify' $ \ud ->
          ud { UD.upkeep = lmap (const $ upkeep { U.workDescription = es }) (UD.upkeep ud) } ,
      textareaRowEditing "Doporučení" (SetValue $ U.recommendation upkeep) $
        \es -> modify' $ \ud ->
          ud { UD.upkeep = lmap (const $ upkeep { U.recommendation = es }) (UD.upkeep ud) } ], 5)
    else ([], 6)

  toggleNote = let
    newClose uc = uc { UD.displayedNote = newDisplayedNote } where
      newDisplayedNote = case UD.displayedNote uc of
        Note -> EndNote
        EndNote -> Note
        x -> x
    in modify' $ \ud -> ud { UD.upkeepPageMode = mapEither newClose (UD.upkeepPageMode ud) }
  (getNote, setNote, noteHeaders) = case displayedNoteFlag of
    Note -> let
      noteActive = [ 
        strong "Poznámka" ,
        A.a''' ((class' "my-text-right") { onClick = Defined . const $ toggleNote } ) "Koncová poznámka" ]
      in (UM.upkeepMachineNote, \t um -> um { UM.upkeepMachineNote = t }, noteActive)
    EndNote -> let
      endNoteActive = [
        A.a''' (mkAttrs { onClick = Defined . const $ toggleNote }) "Poznámka" ,
        strong' (class' "my-text-right") "Koncová poznámka" ]
      in (UM.endNote, \t um -> um { UM.endNote = t }, endNoteActive)
    NoChoice -> let
      onlyNote = [strong "Poznámka"]
      in (UM.upkeepMachineNote, \t um -> um { UM.upkeepMachineNote = t }, onlyNote)
    
  upkeepMachineRow :: (M.MachineId, M.Machine, MT.MachineType, US.UpkeepSequence) -> DOMElement
  upkeepMachineRow (machineId, machine', machineType, nextUpkeepSequence) = let

    mkRow columns = div' (class' "form-group") columns
    findMachineById (_,id') = machineId == id'
    checkedUpkeepMachine = find findMachineById upkeepMachines
    uncheckedUpkeepMachine = find findMachineById uncheckedMachines
    checkedMachineIds = map snd upkeepMachines

    (machine, updateUpkeepMachine, editing) = case (checkedUpkeepMachine, uncheckedUpkeepMachine) of
      (Just(checkedMachine), Nothing) -> let 
        setter :: UM.UpkeepMachine -> Fay ()
        setter upkeepMachine = let
          ums = map (\(um @ (_,machineId')) -> if machineId' == machineId
            then (upkeepMachine, machineId')
            else um) upkeepMachines 
          in setUpkeep (upkeep, ums) rawUpkeepDate
        in (checkedMachine, setter, Editing) 
      (Nothing, Just(uncheckedMachine)) ->
        (uncheckedMachine, const $ return (), Display)

    machineToggleCheckedLink = let
      linkText = MT.machineTypeName machineType <> additionalText where
        machinesWithSameType = filter 
          (\(_,_,machineType',_) -> 
            MT.machineTypeName machineType' == MT.machineTypeName machineType)
          machines
        machinesWithLabels = filter
          (\(_,machine'',_,_) -> M.note machine'' /= "")
          machinesWithSameType
        useLabels = length machinesWithSameType == length machinesWithLabels
        getAdditionalText = if useLabels then M.note else M.serialNumber 
        additionalText = if length machinesWithSameType == 1 
          then "" 
          else " - " <> getAdditionalText machine'
      clickHandler = let
        (newCheckedMachines, newUncheckedMachines) = toggle (
          upkeepMachines ,
          uncheckedMachines )
          (\(_,machineId') -> machineId' == machineId)
        in setNotCheckedMachines newCheckedMachines newUncheckedMachines
      link = A.a''
        (mkAttrs {onClick = Defined $ const clickHandler} )
        (A.mkAAttrs)
        linkText
      icon = if elem machineId checkedMachineIds
        then G.okCircle
        else span ([]::[DOMElement])
      innerRow = B.row [
        B.col (B.mkColProps 2) icon, 
        B.col (B.mkColProps 10) link]
      in B.col (B.mkColProps machineColsSize) innerRow

    recordedMileage = B.col (B.mkColProps 2) $ input editing False
      (DefaultValue $ showInt $ UM.recordedMileage $ fst machine) $ eventInt' 
        (const True)
        (\i -> do
          let newValidation = V.remove (V.MthNumber machineId) validation
          modify' $ \ud -> ud { UD.validation = newValidation }
          updateUpkeepMachine $ ((fst machine) { UM.recordedMileage = i })) 
        (const $ modify' $ \ud -> ud { UD.validation = V.add (V.MthNumber machineId) validation })

    warranty = B.col (B.mkColProps 1) $ checkbox editing (UM.warrantyUpkeep $ fst machine) $ \warrantyUpkeep' ->
      updateUpkeepMachine $ (fst machine) { UM.warrantyUpkeep = warrantyUpkeep' }

    note = B.col (B.mkColProps noteColsSize) $ 
      textarea editing False (SetValue . getNote . fst $ machine) $ \es ->
        updateUpkeepMachine $ setNote es (fst machine)

    nextUpkeepSequenceField = B.col (B.mkColProps 2) $ "Další servis: " <> US.label_ nextUpkeepSequence

    in mkRow $ if closeUpkeep'
      then [machineToggleCheckedLink, recordedMileage, warranty, note]
      else [machineToggleCheckedLink, nextUpkeepSequenceField, note]

  datePicker = let
    modifyDatepickerDate newDate = modify' $ \upkeepData -> upkeepData {
      UD.upkeepDatePicker = lmap (\t -> lmap (const newDate) t) (UD.upkeepDatePicker upkeepData)}
    setPickerOpenness open = modify' $ \upkeepData -> upkeepData {
      UD.upkeepDatePicker = lmap (\t -> rmap (const open) t) (UD.upkeepDatePicker upkeepData)}
    setDate date = case date of
      Right date' -> setUpkeep (upkeep { U.upkeepDate = date' }, upkeepMachines) $ displayDate date'
      Left text' -> setUpkeep (upkeep, upkeepMachines) text'
    dateValue = if (displayDate $ U.upkeepDate upkeep) == rawUpkeepDate
      then Right $ U.upkeepDate upkeep
      else Left rawUpkeepDate
    in DP.datePicker Editing upkeepDatePicker' modifyDatepickerDate setPickerOpenness dateValue setDate

  dateRow = oneElementRow "Datum" datePicker

  employeeSelectRows = 
    multipleInputs "Servisman" "Další servisman" OrderingInvisible setList inputControl elems newField where
      setList employeeIds = modify' $ \ud -> ud { UD.selectedEmployees = employeeIds }
      inputControl employee' setEmployee' = fst $
        nullDropdown employees E.name employee' setEmployee'
      elems = selectedEmployees
      newField = Nothing

  formHeader = div' (class' "form-group") $ [
    B.col (B.mkColProps machineColsSize) $ div $ B.row [B.col (B.mkColProps 2) "", 
      B.col (B.mkColProps 10) $ strong "Stroj" ]] ++ (if closeUpkeep' then [
    B.col (B.mkColProps 2) $ strong "Motohodiny" ,
    B.col (B.mkColProps 1) $ strong "Záruka" ] else [
      B.col (B.mkColProps 2) $ strong "Typ servisu" ]) ++ [
    B.col (B.mkColProps noteColsSize) noteHeaders]

  companyNameHeader =  B.row $ B.col (B.mkColProps 12) $ h2 pageHeader
  validationMessages'' = V.messages validation
  validationMessages' = if (null upkeepMachines)
    then ["V servisu musí figurovat alespoň jeden stroj."]
    else []
  validationMessages = validationMessages'' ++ validationMessages' ++ 
    (if displayDate (U.upkeepDate upkeep) == rawUpkeepDate
      then []
      else ["Musí být nastaveno správně datum."])
  validationGrid = validationHtml validationMessages
  submitButtonRow = oneElementRow "" (button $ null validationMessages)

  mkGrid :: [DOMElement] -> DOMElement -> DOMElement
  mkGrid columns anotherGrid = div $ (form' (class' "form-horizontal") $ B.grid columns) : anotherGrid : []

  in mkGrid upkeepFormRows validationGrid
