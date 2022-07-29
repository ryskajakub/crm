{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Crm.Page.Upkeep (
  upkeepNew ,
  upkeepDetail ,
  calledUpkeeps ,
  plannedUpkeeps ) where

import           Data.Text                        (fromString, Text, showInt, (<>))
import           Prelude                          hiding (div, span, id)
import qualified Prelude                          as Prelude
import           Data.Var (Var, modify)
import           Data.Maybe                       (mapMaybe)
import           FFI (Defined(..))

import           HaskellReact                     as HR
import qualified HaskellReact.Bootstrap           as B
import qualified HaskellReact.Bootstrap.Button    as BTN
import qualified HaskellReact.Bootstrap.Glyphicon as G
import qualified HaskellReact.Tag.Hyperlink       as A
import qualified HaskellReact.Bootstrap.Nav       as BN

import qualified Crm.Shared.Company               as C
import qualified Crm.Shared.Machine               as M
import qualified Crm.Shared.MachineType           as MT
import qualified Crm.Shared.MachineKind           as MK
import qualified Crm.Shared.Upkeep                as U
import qualified Crm.Shared.Employee              as E
import qualified Crm.Shared.UpkeepMachine         as UM
import qualified Crm.Shared.UpkeepSequence        as US
import qualified Crm.Shared.Upkeep                as U

import qualified Crm.Data.Data                    as D
import qualified Crm.Data.UpkeepData              as UD
import qualified Crm.Component.DatePicker         as DP
import qualified Crm.Validation                   as V
import qualified Crm.Router                       as R
import           Crm.Server                       (createUpkeep, updateUpkeep)
import           Crm.Component.Form
import           Crm.Component.Navigation         as N
import           Crm.Helpers
import           Crm.Types                        (DisplayedNote (..))

calledUpkeeps :: 
  R.CrmRouter -> 
  [[(U.UpkeepId, U.Upkeep, C.CompanyId, C.Company, [(M.MachineId, Text, Text, MK.MachineKindEnum)], [E.Employee'])]] -> 
  (DOMElement, Fay ())
calledUpkeeps router upkeeps = let
  upkeeps' = (flip map) upkeeps $ \outer -> (flip map) outer $ \(a, b, c, d, e, f) -> (a, b, Nothing, c, d, e, f)
  in plannedUpkeeps'' "Volané servisy" (p [ text2DOM "Seznam firem, kam bylo voláno a čeká se na kontakt." ]) router upkeeps'

plannedUpkeeps ::
  R.CrmRouter -> 
  [[(U.UpkeepId, U.Upkeep, Maybe U.UpkeepId, C.CompanyId, C.Company, [(M.MachineId, Text, Text, MK.MachineKindEnum)], [E.Employee'])]] -> 
  (DOMElement, Fay ())
plannedUpkeeps = plannedUpkeeps'' "Naplánované servisy" $ p [ text2DOM "Seznam naplánovaných servisů. Tady můžeš buď servis ", strong "přeplánovat", text2DOM ", pokud je třeba u naplánovaného změnit datum a podobně, nebo můžeš servis uzavřít, to se dělá potom co je servis fyzicky hotov a přijde ti servisní list." ]

plannedUpkeeps'' ::
  Renderable a =>
  Text ->
  a ->
  R.CrmRouter -> 
  [[(U.UpkeepId, U.Upkeep, Maybe U.UpkeepId, C.CompanyId, C.Company, [(M.MachineId, Text, Text, MK.MachineKindEnum)], [E.Employee'])]] -> 
  (DOMElement, Fay ())
plannedUpkeeps'' pageTitle pageAdvice router upkeepCompanies = let
  mkTable data' = B.table [head', body] where
    head' = thead $ tr [
      th G.user ,
      th "Název firmy" ,
      th "Adresa" ,
      th "Poznámky" ,
      th "Datum" ,
      th $ B.tooltip (B.TooltipData "tooltip" (Defined "left") (Defined "Přeplánovat") Undefined) G.edit ,
      th $ B.tooltip (B.TooltipData "tooltip" (Defined "right") (Defined "Uzavřít") Undefined) G.check ,
      th $ "List" ]
    body = tbody $ map (\(upkeepId, upkeep, superUpkeepId, companyId, company, notes, employees) -> let

      (cName, cAddress) = maybe ([R.link
        (C.companyName company)
        (R.companyDetail companyId)
        router], [C.companyAddress company]) (const ([], [])) superUpkeepId

      mkNote = map $ \(machineId, machineTypeName, note, _) -> li $ [
        R.link machineTypeName (R.machineDetail machineId) router ,
        text2DOM " : " , text2DOM note]
      rowBackgroundColor = case notes of
        ((_,_,_,mk):_) -> class'' . backgroundForKind $ mk
        _ -> mkAttrs
      in tr' rowBackgroundColor [
        td' (class'' ["first-cell", ""]) . mkColours . map snd $ employees ,
        td cName ,
        td cAddress ,
        td . (ul' (class' "list-unstyled")) . mkNote $ notes ,
        let 
          date = U.upkeepDate upkeep
          in td $ R.link 
            (displayDate date)
            (R.dailyPlan date Nothing)
            router ,
        td $ R.link
          (B.tooltip (B.TooltipData "tooltip" (Defined "bottom") (Defined "Přeplánovat") Undefined) G.edit)
          (R.replanUpkeep upkeepId)
          router,
        td $ R.link
          (B.tooltip (B.TooltipData "tooltip" (Defined "bottom") (Defined "Uzavřít") Undefined) G.check)
          (R.upkeepDetail upkeepId)
          router ,
        td $ A.a ("/ts/list?id=" <> (fromString . show . U.getUpkeepId $ upkeepId)) "List"]) data'

  pageInfo' = pageInfo pageTitle $ Just pageAdvice

  [compressorsTable, othersTable, smallCompaniesTable] = 
    map mkTable upkeepCompanies
  pills = ul' (class'' ["nav", "nav-pills"]) [
    li' (class' "active") . B.pill (click (return ())) "we" $ "Servisujeme my" ,
    li . B.pill (click (return ())) "others" $ "Servisují jiné firmy" ,
    li . B.pill (click (return ())) "small-companies" $ "Malé firmy" ]
  tables = div' (class' "tab-content") [
    div' (mkAttrs { id = Defined "we" , className = Defined "tab-pane active" }) compressorsTable ,
    div' (mkAttrs { id = Defined "others" , className = Defined "tab-pane" }) othersTable ,
    div' (mkAttrs { id = Defined "small-companies" , className = Defined "tab-pane" }) smallCompaniesTable ]
  in (B.grid $ B.row $
    pageInfo' ++
    [B.col (B.mkColProps 12) . main $ [pills, tables], B.fullCol "zelená - šroubové, žlutá - vývěvy"] , initializeTooltip)


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


mkSubmitButton :: 
  Renderable a => 
  a -> 
  Fay () -> 
  Bool -> 
  DOMElement
mkSubmitButton buttonLabel handler enabled = let
  basicButtonProps = BTN.buttonProps {
    BTN.bsStyle = Defined "primary" }
  buttonProps = if enabled
    then basicButtonProps { BTN.onClick = Defined $ const handler }
    else basicButtonProps { BTN.disabled = Defined True }
  in BTN.button' buttonProps buttonLabel


upkeepDetail :: 
  R.CrmRouter -> 
  Var D.AppState ->
  Maybe U.UpkeepId ->
  U.Upkeep' -> 
  DP.DatePickerData -> 
  [UM.UpkeepMachine'] -> 
  [(M.MachineId, M.Machine, MT.MachineType, US.UpkeepSequence)] -> 
  C.CompanyId -> -- ^ company id
  [E.Employee'] -> 
  [Maybe E.EmployeeId] -> 
  V.Validation -> 
  DisplayedNote -> 
  DOMElement
upkeepDetail router appState superUpkeepId upkeep3 datePicker notCheckedMachines 
    machines companyId employees selectedEmployees v dnf =
  upkeepForm appState router ("Uzavřít " <> taskName superUpkeepId) upkeep2 datePicker notCheckedMachines 
    machines submitButton True employees selectedEmployees v dnf companyId []
      where
        (_,upkeep,upkeepMachines) = upkeep3
        upkeep2 = (upkeep,upkeepMachines)
        submitButton = let
          closeUpkeepHandler = updateUpkeep
            upkeep3
            (mapMaybe Prelude.id selectedEmployees)
            (R.navigate (R.maintenances companyId) router)
            router
          in mkSubmitButton 
            [span G.plus , span " Uzavřít"]
            closeUpkeepHandler


taskName :: Maybe a -> Text
taskName = maybe "servis" (const "pudúkol servisu")


upkeepNew :: 
  R.CrmRouter -> 
  Var D.AppState ->
  Maybe U.UpkeepId ->
  (U.Upkeep, [UM.UpkeepMachine']) -> 
  DP.DatePickerData -> 
  [UM.UpkeepMachine'] -> 
  [(M.MachineId, M.Machine, MT.MachineType, US.UpkeepSequence)] -> -- ^ machine ids -> machines  
  Maybe U.UpkeepId -> 
  C.CompanyId ->
  [E.Employee'] -> 
  [Maybe E.EmployeeId] -> 
  V.Validation -> 
  DOMElement
upkeepNew router appState upkeepSupertaskId upkeep datePicker notCheckedMachines
    machines upkeepIdentification companyId es se v =
  upkeepForm appState router pageHeader upkeep datePicker notCheckedMachines
      machines submitButton False es se v NoChoice companyId addButton where

    (upkeepU, upkeepMachines) = upkeep
    taskName' = taskName upkeepSupertaskId

    (pageHeader, submitButton, addButton) = case upkeepIdentification of
      Nothing -> let
        newUpkeepHandler = createUpkeep
          (upkeepU, upkeepMachines, mapMaybe Prelude.id se, upkeepSupertaskId)
          (R.navigate R.plannedUpkeeps router)
          router
        button = mkSubmitButton
          [G.plus , text2DOM " Naplánovat"]
          newUpkeepHandler
        in ("Naplánovat " <> taskName', button, [])
      Just upkeepId -> let
        replanUpkeepHandler = updateUpkeep
          (upkeepId, upkeepU, upkeepMachines)
          (mapMaybe Prelude.id se)
          (R.navigate R.plannedUpkeeps router)
          router
        addSubtaskButton =
          form' (class' "navbar-form") $
            BTN.button' (BTN.buttonProps {
              BTN.onClick = Defined $ const $ R.navigate (R.addUpkeepSubtask upkeepId) router })
            [G.plus, text2DOM " Přidat dílčí úkon servisu" ]
        button = mkSubmitButton
          [text2DOM "Přeplánovat"]
          replanUpkeepHandler
        in ("Přeplánovat " <> taskName', button, maybe (if (not . U.setDate $ upkeepU) then [addSubtaskButton] else []) (const []) upkeepSupertaskId)


modifyUpkeepClose :: (UD.UpkeepPageMode -> UD.UpkeepPageMode) -> UD.UpkeepPageMode -> UD.UpkeepPageMode
modifyUpkeepClose f (uc @ (UD.UpkeepClose {})) = f uc
modifyUpkeepClose _ x = x


upkeepForm :: 
  Var D.AppState -> 
  R.CrmRouter ->
  Text -> -- ^ page header
  (U.Upkeep, [(UM.UpkeepMachine')]) ->
  DP.DatePickerData ->
  [(UM.UpkeepMachine')] ->
  [(M.MachineId, M.Machine, MT.MachineType, US.UpkeepSequence)] -> -- ^ machine ids -> machines
  (Bool -> DOMElement) -> -- ^ submit button
  Bool -> -- ^ display the mth input field
  [E.Employee'] ->
  [Maybe E.EmployeeId] ->
  V.Validation ->
  DisplayedNote ->
  C.CompanyId ->
  [DOMElement] ->
  DOMElement
upkeepForm appState router pageHeader (upkeep, upkeepMachines) upkeepDatePicker' uncheckedMachines 
    machines button closeUpkeep' employees selectedEmployees validation displayedNoteFlag companyId
    addUpkeepSubtaskElement = let
  rawUpkeepDate = DP.rawText upkeepDatePicker'
  upkeepFormRows = 
    formHeader :
    (map upkeepMachineRow machines ++ 
    [setDateCheckboxRow] ++
    [dateRow] ++ 
    employeeSelectRows ++
    closeUpkeepRows ++ 
    [submitButtonRow])

  modify' :: (UD.UpkeepData -> UD.UpkeepData) -> Fay ()
  modify' fun = modify appState $ \appState' -> let
    newState = case D.navigation appState' of
      D.UpkeepScreen ud -> D.UpkeepScreen $ fun ud
    in appState' { D.navigation = newState }

  setUpkeep :: (U.Upkeep, [UM.UpkeepMachine']) -> Fay ()
  setUpkeep modifiedUpkeep = modify' $ \upkeepData ->
    upkeepData { UD.upkeep = modifiedUpkeep }
  
  setNotCheckedMachines :: [UM.UpkeepMachine'] -> [UM.UpkeepMachine'] -> Fay ()
  setNotCheckedMachines checkedMachines notCheckedMachines' = modify' $ 
    \upkeepData -> upkeepData { 
      UD.upkeep = (upkeep, checkedMachines) ,
      UD.notCheckedMachines = notCheckedMachines' }

  textareaRowEditing' int = textareaRow' int Editing
  inputRowEditing = inputRow Editing

  machineColsSize = 4
  (closeUpkeepRows, noteColsSize) = let
    workDescription =
      textareaRowEditing' 5 "Popis servisu" (SetValue . U.workDescription $ upkeep) $
        \es -> modify' $ \ud ->
          ud { UD.upkeep = lmap (const $ upkeep { U.workDescription = es }) (UD.upkeep ud) } 
    in if closeUpkeep'
      then ([inputRowEditing "Délka práce"
          (SetValue $ U.workHours upkeep) $ \es -> modify' $ \ud ->
            ud { UD.upkeep = lmap (const $ upkeep { U.workHours = es }) (UD.upkeep ud) } ,
        workDescription ,
        textareaRowEditing' 5 "Doporučení" (SetValue . U.recommendation $ upkeep) $
          \es -> modify' $ \ud ->
            ud { UD.upkeep = lmap (const $ upkeep { U.recommendation = es }) (UD.upkeep ud) } ], 5)
      else ([workDescription], 5)

  toggleNote = let
    newClose uc = uc { UD.displayedNote = newDisplayedNote } where
      newDisplayedNote = case UD.displayedNote uc of
        Note -> EndNote
        EndNote -> Note
        x -> x
    in modify' $ \ud -> ud { UD.upkeepPageMode = modifyUpkeepClose newClose (UD.upkeepPageMode ud) }
  (getNote, setNote, noteHeaders) = let
    noteText = strong "Plánované úkony"
    endNoteText = strong "Závěry po servisu"
    in case displayedNoteFlag of
      Note -> let
        noteActive = [ 
          noteText ,
          A.a''' ((class' "my-text-right") { onClick = Defined . const $ toggleNote } ) endNoteText ]
        in (UM.upkeepMachineNote, \t um -> um { UM.upkeepMachineNote = t }, noteActive)
      EndNote -> let
        endNoteActive = [
          A.a''' (mkAttrs { onClick = Defined . const $ toggleNote }) noteText ,
          strong' (class' "my-text-right") endNoteText ]
        in (UM.endNote, \t um -> um { UM.endNote = t }, endNoteActive)
      NoChoice -> let
        onlyNote = [noteText]
        in (UM.upkeepMachineNote, \t um -> um { UM.upkeepMachineNote = t }, onlyNote)
    
  upkeepMachineRow :: (M.MachineId, M.Machine, MT.MachineType, US.UpkeepSequence) -> DOMElement
  upkeepMachineRow (machineId, machine', machineType, _) = let

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
          in setUpkeep (upkeep, ums)
        in (checkedMachine, setter, Editing) 
      (Nothing, Just(uncheckedMachine)) ->
        (uncheckedMachine, const $ return (), Display)

    (nextFieldOffset, showMileage) = case MT.kind machineType of
      MK.RotaryScrewCompressor | closeUpkeep' -> (Undefined, True)
      _ -> (Defined 2, False)

    machineToggleCheckedLink = let
      linkText = displayFullMachine machine' machineType
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

    warrantyRepair = B.col (B.ColProps 1 nextFieldOffset) [warranty, upkeepTypeDropdown] where
      warranty = checkbox editing (UM.warrantyUpkeep . fst $ machine) $ \warrantyUpkeep' ->
        updateUpkeepMachine $ (fst machine) { UM.warrantyUpkeep = warrantyUpkeep' }
      displayUpkeepType upkeepType = text2DOM $ case upkeepType of
        UM.Regular -> "S"
        UM.Repair -> "O"
        UM.Check -> "K"
        UM.Installation -> "I"
        UM.General -> "G"
      upkeepTypeDropdown = div' (class' "repair") $ case editing of
        Editing -> dropdown
          (UM.allUpkeepTypes `zip` UM.allUpkeepTypes)
          displayUpkeepType
          (UM.upkeepType . fst $ machine) $
          \upkeepType' -> updateUpkeepMachine $ (fst machine) { UM.upkeepType = upkeepType' }
        Display -> text2DOM ""

    note = B.col (B.mkColProps noteColsSize) $ 
      textarea' 5 editing False (SetValue . getNote . fst $ machine) $ \es ->
        updateUpkeepMachine $ setNote es (fst machine)

    in mkRow $ if closeUpkeep'
      then [machineToggleCheckedLink] ++ (if showMileage then [recordedMileage] else []) ++ [warrantyRepair, note]
      else [machineToggleCheckedLink, warrantyRepair, note]

  datePicker = let
    modifyDatepickerDate newDpd = modify' $ \ud -> ud { UD.upkeepDatePicker = newDpd }
    setDate date' = setUpkeep $ (upkeep { U.upkeepDate = date' }, upkeepMachines)
    in DP.datePicker' Editing upkeepDatePicker' modifyDatepickerDate (U.upkeepDate upkeep) setDate

  setDateCheckbox = let
    mkContainer = div' (class' "editing-checkbox")
    in mkContainer $ checkbox Editing (U.setDate upkeep) (\b -> setUpkeep $ (upkeep { U.setDate = b }, upkeepMachines))
  setDateCheckboxRow = oneElementRow "Zavoláno" setDateCheckbox
  dateRow = oneElementRow "Datum" datePicker

  employeeSelectRows = 
    multipleInputs "Servisman" ["Hlavní servisman"] "Další servisman" OrderingInvisible setList inputControl elems newField where
      setList employeeIds = modify' $ \ud -> ud { UD.selectedEmployees = employeeIds }
      inputControl employee' setEmployee' = fst $
        nullDropdown employees (text2DOM . E.name) employee' setEmployee'
      elems = selectedEmployees
      newField = Nothing

  formHeader = div' (class' "form-group") $
    (B.col (B.mkColProps machineColsSize) $ div $ B.row [
      B.col (B.mkColProps 2) "", 
      B.col (B.mkColProps 10) $ strong "Stroj"]) :
    [B.col (B.mkColProps 2) $ strong $ if closeUpkeep' then "Motohodiny" else ""] ++
    [B.col (B.mkColProps 1) $ strong "Záruka, Oprava*"] ++
    [B.col (B.mkColProps noteColsSize) noteHeaders]

  upkeepPageHeader = B.row $ pageInfo 
    pageHeader 
    (Just $ [p [ text2DOM "Políčka ", strong "Popis servisu", text2DOM " a ", strong "Poznámka", text2DOM " umožňují jednoduché formátování. ", text2DOM basicMarkupInfo ] , 
     p [text2DOM "Popis typů servisu: Pravidelný ", strong "S", text2DOM "ervis, ", strong "O", text2DOM "prava, ", strong "K", text2DOM "ontrola." ]])
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

  myHeader = (B.fullRow (BN.nav $
    N.backToCompany companyId router : addUpkeepSubtaskElement )) :
    [upkeepPageHeader]

  mkGrid :: [DOMElement] -> DOMElement -> DOMElement
  mkGrid columns anotherGrid = div $ [B.grid myHeader] ++ [div' (class'' ["form-horizontal"]) $ B.grid columns] ++ [anotherGrid]

  in mkGrid upkeepFormRows validationGrid
