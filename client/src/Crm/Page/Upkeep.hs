{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Page.Upkeep (
  upkeepNew ,
  upkeepDetail ,
  plannedUpkeeps ) where

import "fay-base" Data.Text (fromString, unpack, pack, showInt, Text)
import "fay-base" Prelude hiding (div, span, id)
import Data.Var (Var, modify)
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
import qualified Crm.Shared.UpkeepMachine as UM
import qualified Crm.Data as D
import qualified Crm.Component.DatePicker as DP
import Crm.Server (createUpkeep, updateUpkeep)
import Crm.Router (CrmRouter, link, companyDetail, closeUpkeep, navigate, maintenances)
import Crm.Helpers (displayDate, parseSafely, lmap)

plannedUpkeeps :: CrmRouter
               -> [(U.UpkeepId, U.Upkeep, C.CompanyId, C.Company)]
               -> DOMElement
plannedUpkeeps router upkeepCompanies = let
  head' = thead $ tr [
    th "Název firmy" ,
    th "Datum" ,
    th "Uzavřít" ]
  body = tbody $ map (\(upkeepId, upkeep, companyId, company) ->
    tr [
      td $ link
        (pack $ C.companyName company)
        (companyDetail companyId)
        router ,
      td $ displayDate $ U.upkeepDate upkeep ,
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

upkeepDetail :: CrmRouter
             -> Var D.AppState
             -> U.Upkeep'
             -> DP.DatePicker
             -> [UM.UpkeepMachine']
             -> [(M.MachineId, M.Machine, C.CompanyId, MT.MachineTypeId, MT.MachineType)] 
             -> C.CompanyId -- ^ company id
             -> DOMElement
upkeepDetail router appState upkeep3 datePicker notCheckedMachines machines companyId =
  upkeepForm appState upkeep2 datePicker notCheckedMachines machines submitButton True
    where
      (_,upkeep,upkeepMachines) = upkeep3
      upkeep2 = (upkeep,upkeepMachines)
      submitButton = let
        closeUpkeepHandler = updateUpkeep
          upkeep3
          (navigate (maintenances companyId) router)
        buttonProps = BTN.buttonProps {
          BTN.bsStyle = Defined "primary" ,
          BTN.onClick = Defined $ const closeUpkeepHandler }
        in BTN.button' buttonProps [ G.plus , text2DOM " Uzavřít" ]

upkeepNew :: CrmRouter
          -> Var D.AppState
          -> (U.Upkeep, [UM.UpkeepMachine'])
          -> DP.DatePicker
          -> [UM.UpkeepMachine']
          -> [(M.MachineId, M.Machine, C.CompanyId, MT.MachineTypeId, MT.MachineType)] -- ^ machine ids -> machines
          -> C.CompanyId -- ^ company id
          -> DOMElement
upkeepNew router appState newUpkeep datePicker notCheckedMachines machines companyId = 
  upkeepForm appState newUpkeep datePicker notCheckedMachines machines submitButton False
    where
      submitButton = let
        newUpkeepHandler = createUpkeep
          newUpkeep
          companyId
          (navigate (maintenances companyId) router)
        buttonProps = BTN.buttonProps {
          BTN.bsStyle = Defined "primary" ,
          BTN.onClick = Defined $ const newUpkeepHandler }
        button = BTN.button' buttonProps [ G.plus , text2DOM " Naplánovat" ]
        in button

upkeepForm :: Var D.AppState
           -> (U.Upkeep, [UM.UpkeepMachine'])
           -> DP.DatePicker -- ^ datepicker openness
           -> [UM.UpkeepMachine']
           -> [(M.MachineId, M.Machine, C.CompanyId, MT.MachineTypeId, MT.MachineType)] 
              -- ^ machine ids -> machines
           -> DOMElement -- ^ submit button
           -> Bool -- ^ display the mth input field
           -> DOMElement
upkeepForm appState (upkeep', upkeepMachines) upkeepDatePicker
    notCheckedMachines'' machines button closeUpkeep' = let
  setUpkeep :: (U.Upkeep,[UM.UpkeepMachine']) -> Maybe [UM.UpkeepMachine'] -> Fay ()
  setUpkeep upkeep notCheckedMachines' = modify appState (\appState' -> let
    newAppState oldNavigation = let
      newNavigation = oldNavigation { D.upkeep = upkeep }
      newNavigation' = case notCheckedMachines' of
        Just(x) -> newNavigation { D.notCheckedMachines = x }
        _ -> newNavigation
      in appState' { D.navigation = newNavigation' }
    in case D.navigation appState' of
      upkeepClose' @ (D.UpkeepClose _ _ _ _ _ _) -> newAppState upkeepClose'
      upkeepNew' @ (D.UpkeepNew _ _ _ _ _) -> newAppState upkeepNew'
      _ -> appState')
  machineRow (machineId,_,_,_,machineType) = let
    findMachineById (_,id') = machineId == id'
    thisUpkeepMachine = find findMachineById upkeepMachines
    thatUpkeepMachine = find findMachineById notCheckedMachines''
    checkedMachineIds = map snd upkeepMachines
    rowProps = if elem machineId checkedMachineIds
      then class' "bg-success"
      else mkAttrs
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
                in setUpkeep (upkeep', newUpkeepMachines) Nothing 
              _ -> return (),
          I.defaultValue = defaultValue upkeepMachine }
        (_, Just(upkeepMachine)) -> I.mkInputProps {
          I.defaultValue = defaultValue upkeepMachine ,
          I.disabled = Defined True }
        _ -> I.mkInputProps { -- this shouldn't happen, really
          I.disabled = Defined True }
      in B.col (B.mkColProps rowWidth) $ inputType inputProps
    machineToggleLink = let
      content = span $ pack $ MT.machineTypeName machineType
      clickHandler = let
        (newCheckedMachines, newNotCheckedMachines) = toggle (
          upkeepMachines ,
          notCheckedMachines'' )
          (\(_,machineId') -> machineId' == machineId)
        newUpkeep = (upkeep' , newCheckedMachines)
        in setUpkeep newUpkeep $ Just newNotCheckedMachines
      link' = A.a''
        (mkAttrs{onClick = Defined $ const clickHandler})
        (A.mkAAttrs)
        content
      in B.col (B.mkColProps (if closeUpkeep' then 4 else 6)) link'
    recordedMileageField = field parseSafely (\v (um,id') -> (um { UM.recordedMileage = v },id') ) 
      (showInt . UM.recordedMileage) I.input 2
    noteField = field (\a -> Just a) (\note (um,id') -> (um { UM.upkeepMachineNote = unpack note },id')) 
      (pack . UM.upkeepMachineNote) I.textarea 6
    rowItems = if closeUpkeep'
      then [machineToggleLink, recordedMileageField, noteField]
      else [machineToggleLink, noteField]
    in B.row' rowProps rowItems
  submitButton = B.col ((B.mkColProps 6){ B.mdOffset = Defined 6 }) button
  dateRow = B.row [
    B.col (B.mkColProps 6) "Datum" ,
    B.col (B.mkColProps 6) $ let
      modifyDatepickerDate newDate = modify appState (\appState' -> appState' {
        D.navigation = case D.navigation appState' of
          upkeepNew' @ (D.UpkeepNew _ _ _ _ _) -> upkeepNew' { 
            D.upkeepDatePicker = lmap (const newDate) (D.upkeepDatePicker upkeepNew') }
          upkeepClose' @ (D.UpkeepClose _ _ _ _ _ _) -> upkeepClose' { 
            D.upkeepDatePicker = lmap (const newDate) (D.upkeepDatePicker upkeepClose') }
          _ -> D.navigation appState' })
      setPickerOpenness open = modify appState (\appState' -> appState' {
        D.navigation = case D.navigation appState' of
          upkeep'' @ (D.UpkeepNew _ _ _ _ _) -> upkeep'' { 
            D.upkeepDatePicker = (fst $ D.upkeepDatePicker upkeep'',open) }
          upkeep'' @ (D.UpkeepClose _ _ _ _ _ _) -> upkeep'' { 
            D.upkeepDatePicker = (fst $ D.upkeepDatePicker upkeep'',open) } } )
      displayedDate = U.upkeepDate upkeep'
      setDate date = setUpkeep (upkeep' { U.upkeepDate = date }, upkeepMachines) Nothing
      in DP.datePicker True upkeepDatePicker modifyDatepickerDate 
        setPickerOpenness displayedDate setDate ]
  employeeSelectRow = B.row [
    B.col (B.mkColProps 6) "Servisman" ,
    B.col (B.mkColProps 6) $ let
      elements = [
        li "Ahoj" ,
        li "Čau" ]
      in BD.buttonDropdown "abc" elements ]
  in div $
    B.grid $
      map machineRow machines ++ [dateRow, employeeSelectRow, submitButton]
