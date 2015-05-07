{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Page.Upkeep (
  upkeepNew ,
  upkeepDetail ,
  plannedUpkeeps ) where

import "fay-base" Data.Text (fromString, unpack, pack, showInt, Text, (<>))
import "fay-base" Prelude hiding (div, span, id)
import "fay-base" Data.Var (Var, modify)
import FFI (Defined(Defined))

import HaskellReact as HR
import qualified HaskellReact.Bootstrap as B
import qualified HaskellReact.Bootstrap.Input as I
import qualified HaskellReact.Bootstrap.Button as BTN
import qualified HaskellReact.Bootstrap.Glyphicon as G
import qualified HaskellReact.Tag.Hyperlink as A
import qualified HaskellReact.Bootstrap.ButtonDropdown as BD

import qualified Crm.Shared.Company as C
import qualified Crm.Shared.Machine as M
import qualified Crm.Shared.MachineType as MT
import qualified Crm.Shared.Upkeep as U
import qualified Crm.Shared.Employee as E
import qualified Crm.Shared.UpkeepMachine as UM

import qualified Crm.Data.Data as D
import qualified Crm.Data.UpkeepData as UD
import qualified Crm.Component.DatePicker as DP
import Crm.Server (createUpkeep, updateUpkeep)
import Crm.Router (CrmRouter, link, companyDetail, closeUpkeep, navigate, maintenances)
import Crm.Component.Form (editingInput, editingTextarea, editingCheckbox, formRow)
import qualified Crm.Router as R
import Crm.Helpers (displayDate, parseSafely, lmap, rmap, pageInfo, validationHtml)

plannedUpkeeps :: CrmRouter
               -> [(U.UpkeepId, U.Upkeep, C.CompanyId, C.Company)]
               -> DOMElement
plannedUpkeeps router upkeepCompanies = let
  head' = thead' (row "1") $ tr [
    th' (row "1") "Název firmy" ,
    th' (row "2") "Datum" ,
    th' (row "3") "Přeplánovat" ,
    th' (row "4") "Uzavřít" ]
  body = tbody' (row "2") $ map (\(upkeepId, upkeep, companyId, company) ->
    tr [
      td' (row "1") $ link
        (pack $ C.companyName company)
        (companyDetail companyId)
        router ,
      td' (row "2") $ displayDate $ U.upkeepDate upkeep ,
      td' (row "3") $ link
        "Přeplánovat"
        (R.replanUpkeep upkeepId)
        router,
      td' (row "4") $ link
        "Uzavřít"
        (closeUpkeep upkeepId)
        router ]) upkeepCompanies

  advice = p [ text2DOM "Seznam naplánovaných servisů. Tady můžeš buď servis ", strong "přeplánovat", text2DOM ", pokud je třeba u naplánovaného změnit datum a podobně, nebo můžeš servis uzavřít, to se dělá potom co je servis fyzicky hotov a přijde ti servisní list." ]
  pageInfo' = pageInfo "Naplánované servisy" $ Just advice

  in B.grid $ B.row $
    pageInfo' ++
    [ B.col (B.mkColProps 12) $ main $ B.table [ head', body ]]

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

{- | if the element is in the first list, put it in the other one, if the element
 - is in the other, put in the first list
 -}
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

upkeepDetail :: CrmRouter
             -> Var D.AppState
             -> U.Upkeep'
             -> (DP.DatePicker, Text)
             -> [UM.UpkeepMachine']
             -> [(M.MachineId, M.Machine, C.CompanyId, MT.MachineTypeId, MT.MachineType)] 
             -> C.CompanyId -- ^ company id
             -> [E.Employee']
             -> Maybe E.EmployeeId
             -> DOMElement
upkeepDetail router appState upkeep3 datePicker notCheckedMachines 
    machines companyId employees selectedEmployee =
  upkeepForm appState "Uzavřít servis" upkeep2 datePicker notCheckedMachines 
    machines submitButton True employees selectedEmployee
      where
        (_,upkeep,upkeepMachines) = upkeep3
        upkeep2 = (upkeep,upkeepMachines)
        submitButton = let
          closeUpkeepHandler = updateUpkeep
            (upkeep3, selectedEmployee)
            (navigate (maintenances companyId) router)
          in mkSubmitButton 
            [span' (row "1") G.plus , span' (row "2") " Uzavřít"]
            closeUpkeepHandler

upkeepNew :: CrmRouter
          -> Var D.AppState
          -> (U.Upkeep, [UM.UpkeepMachine'])
          -> (DP.DatePicker, Text)
          -> [UM.UpkeepMachine']
          -> [(M.MachineId, M.Machine, C.CompanyId, MT.MachineTypeId, MT.MachineType)] -- ^ machine ids -> machines
          -> Either C.CompanyId U.UpkeepId
          -> [E.Employee']
          -> Maybe E.EmployeeId
          -> DOMElement
upkeepNew router appState upkeep datePicker notCheckedMachines machines upkeepIdentification es mE = 
  upkeepForm appState pageHeader upkeep datePicker notCheckedMachines machines submitButton False es mE
    where
      (upkeepU, upkeepMachines) = upkeep
      (pageHeader, submitButton) = case upkeepIdentification of 
        Left (companyId) -> let
          newUpkeepHandler = createUpkeep
            (upkeepU, upkeepMachines, mE)
            companyId
            (navigate R.plannedUpkeeps router)
          button = mkSubmitButton
            [G.plus , text2DOM " Naplánovat"]
            newUpkeepHandler
          in ("Naplánovat servis", button)
        Right (upkeepId) -> let
          replanUpkeepHandler = updateUpkeep
            ((upkeepId, upkeepU, upkeepMachines), mE)
            (navigate R.plannedUpkeeps router)
          button = mkSubmitButton
            [text2DOM "Přeplánovat"]
            replanUpkeepHandler
          in ("Přeplánovat servis", button)

upkeepForm :: Var D.AppState
           -> Text -- ^ page header
           -> (U.Upkeep, [UM.UpkeepMachine'])
           -> (DP.DatePicker, Text) -- ^ datepicker, datepicker openness
           -> [UM.UpkeepMachine']
           -> [(M.MachineId, M.Machine, C.CompanyId, MT.MachineTypeId, MT.MachineType)] 
              -- ^ machine ids -> machines
           -> (Bool -> DOMElement) -- ^ submit button
           -> Bool -- ^ display the mth input field
           -> [E.Employee']
           -> Maybe E.EmployeeId
           -> DOMElement
upkeepForm appState pageHeader (upkeep, upkeepMachines) (upkeepDatePicker', rawUpkeepDate)
    notCheckedMachines'' machines button closeUpkeep' employees selectedEmployee = let
  modify' :: (UD.UpkeepData -> UD.UpkeepData) -> Fay ()
  modify' fun = modify appState (\appState' -> let
    newState = case D.navigation appState' of
      D.UpkeepScreen ud -> D.UpkeepScreen $ fun ud
    in appState' { D.navigation = newState } )

  setUpkeepFull :: (U.Upkeep, [UM.UpkeepMachine']) -> Text -> Fay ()
  setUpkeepFull modifiedUpkeep upkeepDateText = modify' (\upkeepData ->
    let dp = fst $ UD.upkeepDatePicker upkeepData
    in upkeepData { 
      UD.upkeep = modifiedUpkeep ,
      UD.upkeepDatePicker = (dp, upkeepDateText) })
  
  setUpkeep :: (U.Upkeep, [UM.UpkeepMachine']) -> Fay ()
  setUpkeep modifiedUpkeep = setUpkeepFull modifiedUpkeep rawUpkeepDate

  setNotCheckedMachines :: [UM.UpkeepMachine'] -> [UM.UpkeepMachine'] -> Fay ()
  setNotCheckedMachines checkedMachines notCheckedMachines' = modify' 
    (\upkeepData -> upkeepData { 
      UD.upkeep = (upkeep, checkedMachines) ,
      UD.notCheckedMachines = notCheckedMachines' })
    
  machineRow (machineId,_,_,_,machineType) = let
    findMachineById (_,id') = machineId == id'
    thisUpkeepMachine = find findMachineById upkeepMachines
    thatUpkeepMachine = find findMachineById notCheckedMachines''
    checkedMachineIds = map snd upkeepMachines
    field :: (Text -> Maybe a) 
          -> (a -> UM.UpkeepMachine' -> UM.UpkeepMachine')
          -> (UM.UpkeepMachine -> Text)
          -> (I.InputProps -> DOMElement)
          -> Int -- ^ row width
          -> Defined Text
          -> DOMElement
    field parseText setValue showValue inputType rowWidth key' = let
      defaultValue upkeepMachine = Defined $ showValue $ fst upkeepMachine
      inputProps = case (thisUpkeepMachine, thatUpkeepMachine) of
        (Just(upkeepMachine), _) -> I.mkInputProps {
          I.onChange = Defined $ \event -> do
            rawValue <- eventValue event
            case parseText rawValue of
              Just(value) -> let
                newUpkeepMachine = setValue value upkeepMachine
                newUpkeepMachines = map (\ um @ (_,machineId') ->
                  if machineId' == machineId
                    then newUpkeepMachine
                    else um) upkeepMachines
                in setUpkeep (upkeep, newUpkeepMachines)
              _ -> return (),
          I.defaultValue = defaultValue upkeepMachine }
        (_, Just(upkeepMachine)) -> I.mkInputProps {
          I.defaultValue = defaultValue upkeepMachine ,
          I.disabled = Defined True }
        _ -> I.mkInputProps { -- this shouldn't happen, really
          I.disabled = Defined True }
      in B.col' (B.mkColProps rowWidth) key' $ inputType inputProps
    machineToggleLink = let
      content = pack $ MT.machineTypeName machineType
      clickHandler = let
        (newCheckedMachines, newNotCheckedMachines) = toggle (
          upkeepMachines ,
          notCheckedMachines'' )
          (\(_,machineId') -> machineId' == machineId)
        in setNotCheckedMachines newCheckedMachines newNotCheckedMachines
      link' = A.a''
        (mkAttrs {onClick = Defined $ const clickHandler} )
        (A.mkAAttrs)
        content
      icon = if elem machineId checkedMachineIds
        then G.okCircle
        else span ([]::[DOMElement])
      innerRow = B.row [B.col' (B.mkColProps 2) (Defined "1") icon, 
        B.col' (B.mkColProps 10) (Defined "2") link']
      in B.col' (B.mkColProps (if closeUpkeep' then 4 else 6)) (Defined "1") innerRow
    recordedMileageField = field parseSafely (\v (um,id') -> (um { UM.recordedMileage = v },id'))
      (showInt . UM.recordedMileage) I.input 2
    warrantyUpkeep = case (thisUpkeepMachine, thatUpkeepMachine) of
      (Just(thisMachine), Nothing) -> 
        editingCheckbox (UM.warrantyUpkeep $ fst thisMachine) (\boolean -> let
          updatedUpkeep = (fst thisMachine) { UM.warrantyUpkeep = boolean }
          ums = map (\(um @ (_,machineId')) -> if machineId' == machineId
            then (updatedUpkeep, machineId')
            else um ) upkeepMachines 
          in setUpkeep (upkeep, ums) ) True
      (Nothing, Just(thatMachine)) ->
        editingCheckbox (UM.warrantyUpkeep $ fst thatMachine) (const $ return ()) False
      _ -> undefined
    warrantyUpkeepRow = B.col' (B.mkColProps 1) (Defined "3") warrantyUpkeep
    noteField = field (\a -> Just a) (\note (um,id') -> (um { UM.upkeepMachineNote = unpack note }, id')) 
      (pack . UM.upkeepMachineNote) I.textarea (if closeUpkeep' then 5 else 6)
    rowItems = if closeUpkeep'
      then [machineToggleLink, recordedMileageField (Defined "2"), 
        warrantyUpkeepRow , noteField (Defined "4")]
      else [machineToggleLink, noteField (Defined "2")]
    in B.row rowItems
  datePicker = let
    modifyDatepickerDate newDate = modify' (\upkeepData -> upkeepData {
      UD.upkeepDatePicker = lmap (\t -> lmap (const newDate) t) (UD.upkeepDatePicker upkeepData)}) 
    setPickerOpenness open = modify' (\upkeepData -> upkeepData {
      UD.upkeepDatePicker = lmap (\t -> rmap (const open) t) (UD.upkeepDatePicker upkeepData)})
    setDate date = case date of
      Right date' -> setUpkeepFull (upkeep { U.upkeepDate = date' }, upkeepMachines) $ displayDate date'
      Left text' -> setUpkeepFull (upkeep, upkeepMachines) text'
    in DP.datePicker True upkeepDatePicker' modifyDatepickerDate 
      setPickerOpenness (Left rawUpkeepDate) setDate
  dateRow = formRow "Datum" datePicker
  employeeSelectRow = formRow "Servisman" (let
    noEmployeeLabel = "---"
    selectedEmployeeName = maybe noEmployeeLabel (\employeeId -> let
      employeeFoundInList = lookup employeeId employees
      in maybe noEmployeeLabel (pack . E.name) employeeFoundInList) selectedEmployee
    selectEmployeeLink eId e = let
      selectEmployeeAction = modify' (\s -> s { UD.selectedEmployee = eId })
      in A.a''' (click selectEmployeeAction) (pack $ E.name e)
    withNoEmployee = (Nothing, E.newEmployee { E.name = unpack noEmployeeLabel }) : (map (lmap Just) employees)
    elements = map (\(eId,e) -> li $ selectEmployeeLink eId e ) withNoEmployee
    buttonLabel = [ text2DOM $ selectedEmployeeName <> " " , span' (class' "caret") "" ]
    in BD.buttonDropdown buttonLabel elements )
  workHoursRow = formRow "Hodiny" $ 
    editingInput (U.workHours upkeep) (eventString >=> \es -> modify' (\ud ->
      ud { UD.upkeep = lmap (const $ upkeep { U.workHours = es }) (UD.upkeep ud) } )) True False
  workDescriptionRow = formRow "Popis práce" $
    editingTextarea (U.workDescription upkeep) (eventString >=> \es -> modify' (\ud ->
      ud { UD.upkeep = lmap (const $ upkeep { U.workDescription = es }) (UD.upkeep ud) })) True False 
  recommendationRow = formRow "Doporučení" $
    editingTextarea (U.recommendation upkeep) (eventString >=> \es -> modify' (\ud ->
      ud { UD.upkeep = lmap (const $ upkeep { U.recommendation = es }) (UD.upkeep ud) })) True False
  closeUpkeepRows = [workHoursRow, workDescriptionRow, recommendationRow]
  additionalRows = if closeUpkeep' then closeUpkeepRows else []
  header = B.row $ [
    B.col (B.mkColProps (if closeUpkeep' then 4 else 6)) $ div $ B.row [B.col (B.mkColProps 2) "", 
      B.col (B.mkColProps 10) $ strong "Stroj" ]] ++ (if closeUpkeep' then [
    B.col (B.mkColProps 2) $ div' (class' "form-group") $ label "Motohodiny" ,
    B.col (B.mkColProps 1) $ strong "Záruka" ] else []) ++ [
    B.col (B.mkColProps (if closeUpkeep' then 5 else 6)) $ div' (class' "form-group") $ label "Poznámka" ]
  companyNameHeader = B.row $ B.col (B.mkColProps 12) $ h2 pageHeader

  validationMessages' = if (null upkeepMachines)
    then ["V servisu musí figurovat alespoň jeden stroj."]
    else []
  validationMessages = validationMessages' ++ (if displayDate (U.upkeepDate upkeep) == rawUpkeepDate
    then []
    else ["Musí být nastaveno správně datum."])
  submitButton = formRow "" (button $ null validationMessages)
  messagesPart = validationHtml validationMessages

  in div $ (form' (class' "form-horizontal") $ B.grid $
    [companyNameHeader] ++
    [header] ++
    map machineRow machines ++ 
    [dateRow, employeeSelectRow] ++ 
    additionalRows ++ 
    [submitButton]) : messagesPart : []
