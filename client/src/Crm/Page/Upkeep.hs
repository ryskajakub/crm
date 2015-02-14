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
import qualified Crm.Router as R
import Crm.Helpers (displayDate, parseSafely, lmap, rmap, editingInput, editingTextarea, 
  editingCheckbox)

plannedUpkeeps :: CrmRouter
               -> [(U.UpkeepId, U.Upkeep, C.CompanyId, C.Company)]
               -> DOMElement
plannedUpkeeps router upkeepCompanies = let
  head' = thead $ tr [
    th "Název firmy" ,
    th "Datum" ,
    th "Přeplánovat" ,
    th "Uzavřít" ]
  body = tbody $ map (\(upkeepId, upkeep, companyId, company) ->
    tr [
      td $ link
        (pack $ C.companyName company)
        (companyDetail companyId)
        router ,
      td $ displayDate $ U.upkeepDate upkeep ,
      td $ link
        "Přeplánovat"
        (R.replanUpkeep upkeepId)
        router,
      td $ link
        "Uzavřít"
        (closeUpkeep upkeepId)
        router ]) upkeepCompanies
  in main $ B.table [ head' , body ]

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
               => Bool
               -> a
               -> Fay ()
               -> DOMElement
mkSubmitButton enabled buttonLabel handler = let
  basicButtonProps = BTN.buttonProps {
    BTN.bsStyle = Defined "primary" }
  buttonProps = if enabled
    then basicButtonProps { BTN.onClick = Defined $ const handler }
    else basicButtonProps { BTN.disabled = Defined True }
  in BTN.button' buttonProps buttonLabel

upkeepDetail :: CrmRouter
             -> Var D.AppState
             -> C.Company
             -> U.Upkeep'
             -> DP.DatePicker
             -> [UM.UpkeepMachine']
             -> [(M.MachineId, M.Machine, C.CompanyId, MT.MachineTypeId, MT.MachineType)] 
             -> C.CompanyId -- ^ company id
             -> [E.Employee']
             -> Maybe E.EmployeeId
             -> DOMElement
upkeepDetail router appState company upkeep3 datePicker notCheckedMachines 
    machines companyId employees selectedEmployee =
  upkeepForm appState company upkeep2 datePicker notCheckedMachines 
    machines submitButton True employees selectedEmployee
      where
        (_,upkeep,upkeepMachines) = upkeep3
        upkeep2 = (upkeep,upkeepMachines)
        submitButton = let
          closeUpkeepHandler = updateUpkeep
            (upkeep3, selectedEmployee)
            (navigate (maintenances companyId) router)
          in mkSubmitButton 
            (not $ null upkeepMachines) 
            [G.plus , text2DOM " Uzavřít"]
            closeUpkeepHandler

upkeepNew :: CrmRouter
          -> Var D.AppState
          -> C.Company
          -> (U.Upkeep, [UM.UpkeepMachine'])
          -> DP.DatePicker
          -> [UM.UpkeepMachine']
          -> [(M.MachineId, M.Machine, C.CompanyId, MT.MachineTypeId, MT.MachineType)] -- ^ machine ids -> machines
          -> Either C.CompanyId U.UpkeepId
          -> [E.Employee']
          -> Maybe E.EmployeeId
          -> DOMElement
upkeepNew router appState company upkeep datePicker notCheckedMachines machines upkeepIdentification es mE = 
  upkeepForm appState company upkeep datePicker notCheckedMachines machines submitButton False es mE
    where
      (upkeepU, upkeepMachines) = upkeep
      mkSubmitButton' = mkSubmitButton (not $ null upkeepMachines)
      submitButton = case upkeepIdentification of 
        Left (companyId) -> let
          newUpkeepHandler = createUpkeep
            (upkeepU, upkeepMachines, mE)
            companyId
            (navigate R.plannedUpkeeps router)
          in mkSubmitButton'
            [G.plus , text2DOM " Naplánovat"]
            newUpkeepHandler
        Right (upkeepId) -> let
          replanUpkeepHandler = updateUpkeep
            ((upkeepId, upkeepU, upkeepMachines), mE)
            (navigate R.plannedUpkeeps router)
          in mkSubmitButton'
            [text2DOM "Přeplánovat"]
            replanUpkeepHandler

upkeepForm :: Var D.AppState
           -> C.Company
           -> (U.Upkeep, [UM.UpkeepMachine'])
           -> DP.DatePicker -- ^ datepicker openness
           -> [UM.UpkeepMachine']
           -> [(M.MachineId, M.Machine, C.CompanyId, MT.MachineTypeId, MT.MachineType)] 
              -- ^ machine ids -> machines
           -> DOMElement -- ^ submit button
           -> Bool -- ^ display the mth input field
           -> [E.Employee']
           -> Maybe E.EmployeeId
           -> DOMElement
upkeepForm appState company (upkeep, upkeepMachines) upkeepDatePicker'
    notCheckedMachines'' machines button closeUpkeep' employees selectedEmployee = let
  modify' :: (UD.UpkeepData -> UD.UpkeepData) -> Fay ()
  modify' fun = modify appState (\appState' -> let
    newState = case D.navigation appState' of
      D.UpkeepScreen ud -> D.UpkeepScreen $ fun ud
    in appState' { D.navigation = newState } )

  setUpkeep :: (U.Upkeep, [UM.UpkeepMachine']) -> Fay ()
  setUpkeep modifiedUpkeep = modify' (\upkeepData -> upkeepData { UD.upkeep = modifiedUpkeep })

  setNotCheckedMachines :: [UM.UpkeepMachine'] -> Fay ()
  setNotCheckedMachines notCheckedMachines' = modify' 
    (\upkeepData -> upkeepData { UD.notCheckedMachines = notCheckedMachines' })
    
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
          -> DOMElement
    field parseText setValue showValue inputType rowWidth = let
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
      in B.col (B.mkColProps rowWidth) $ inputType inputProps
    machineToggleLink = let
      content = pack $ MT.machineTypeName machineType
      clickHandler = let
        (newCheckedMachines, newNotCheckedMachines) = toggle (
          upkeepMachines ,
          notCheckedMachines'' )
          (\(_,machineId') -> machineId' == machineId)
        newUpkeep = (upkeep, newCheckedMachines)
        in do 
          setUpkeep newUpkeep 
          setNotCheckedMachines newNotCheckedMachines
      link' = A.a''
        (mkAttrs {onClick = Defined $ const clickHandler} )
        (A.mkAAttrs)
        content
      icon = if elem machineId checkedMachineIds
        then G.okCircle
        else span ([]::[DOMElement])
      innerRow = B.row [B.col (B.mkColProps 2) icon, B.col (B.mkColProps 10) link']
      in B.col (B.mkColProps (if closeUpkeep' then 4 else 6)) innerRow
    recordedMileageField = field parseSafely (\v (um,id') -> (um { UM.recordedMileage = v },id') )
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
    warrantyUpkeepRow = B.col (B.mkColProps 1) warrantyUpkeep
    noteField = field (\a -> Just a) (\note (um,id') -> (um { UM.upkeepMachineNote = unpack note }, id')) 
      (pack . UM.upkeepMachineNote) I.textarea (if closeUpkeep' then 5 else 6)
    rowItems = if closeUpkeep'
      then [machineToggleLink, recordedMileageField, warrantyUpkeepRow, noteField]
      else [machineToggleLink, noteField]
    in B.row rowItems
  upkeepRow :: (Renderable a, Renderable b) => a -> b -> DOMElement
  upkeepRow labelText content =
    B.row $ div' (class' "form-group") [
      label' (class'' ["control-label", "col-md-3"]) labelText ,
      B.col (B.mkColProps 9) content ]
  submitButton = upkeepRow "" button
  dateRow = upkeepRow "Datum" $ let
    modifyDatepickerDate newDate = modify' (\upkeepData -> upkeepData {
      UD.upkeepDatePicker = lmap (const newDate) (UD.upkeepDatePicker upkeepData)} )
    setPickerOpenness open = modify' (\upkeepData -> upkeepData {
      UD.upkeepDatePicker = rmap (const open) (UD.upkeepDatePicker upkeepData)})
    displayedDate = U.upkeepDate upkeep
    setDate date = setUpkeep (upkeep { U.upkeepDate = date }, upkeepMachines)
    in DP.datePicker True upkeepDatePicker' modifyDatepickerDate 
      setPickerOpenness displayedDate setDate
  employeeSelectRow = upkeepRow "Servisman" (let
    noEmployeeLabel = "---"
    selectedEmployeeName = maybe noEmployeeLabel (\employeeId -> let
      employeeFoundInList = lookup employeeId employees
      in maybe noEmployeeLabel (pack . E.name) employeeFoundInList) selectedEmployee
    selectEmployeeLink eId e = let
      selectEmployeeAction = modify' (\s -> s { UD.selectedEmployee = eId })
      in A.a''' (click selectEmployeeAction) (pack $ E.name e)
    withNoEmployee = (Nothing, E.Employee $ unpack noEmployeeLabel) : (map (lmap Just) employees)
    elements = map (\(eId,e) -> li $ selectEmployeeLink eId e ) withNoEmployee
    buttonLabel = [ text2DOM $ selectedEmployeeName <> " " , span' (class' "caret") "" ]
    in BD.buttonDropdown buttonLabel elements )
  workHoursRow = upkeepRow "Hodiny" $ 
    editingInput (U.workHours upkeep) (eventString >=> \es -> modify' (\ud ->
      ud { UD.upkeep = lmap (const $ upkeep { U.workHours = es }) (UD.upkeep ud) } )) True False
  workDescriptionRow = upkeepRow "Popis práce" $
    editingTextarea (U.workDescription upkeep) (eventString >=> \es -> modify' (\ud ->
      ud { UD.upkeep = lmap (const $ upkeep { U.workDescription = es }) (UD.upkeep ud) })) True False 
  recommendationRow = upkeepRow "Doporučení" $
    editingTextarea (U.recommendation upkeep) (eventString >=> \es -> modify' (\ud ->
      ud { UD.upkeep = lmap (const $ upkeep { U.recommendation = es }) (UD.upkeep ud) })) True False
  closeUpkeepRows = [workHoursRow, workDescriptionRow, recommendationRow]
  additionalRows = if closeUpkeep' then closeUpkeepRows else []
  header = B.row [ 
    B.col (B.mkColProps 4) $ div $ B.row [B.col (B.mkColProps 2) "", 
      B.col (B.mkColProps 10) $ strong "Stroj" ] ,
    B.col (B.mkColProps 2) $ div' (class' "form-group") $ label "Motohodiny" ,
    B.col (B.mkColProps 1) $ strong "Záruka" , 
    B.col (B.mkColProps 5) $ div' (class' "form-group") $ label "Poznámka" ]
  companyNameHeader = B.row $ B.col ((B.mkColProps 9) { B.mdOffset = Defined 3 }) ( h2 $ pack $
    C.companyName company )
  in form' (class' "form-horizontal") $ B.grid $
    [companyNameHeader] ++
    [header] ++
    map machineRow machines ++ 
    [dateRow, employeeSelectRow] ++ 
    additionalRows ++ 
    [submitButton]
