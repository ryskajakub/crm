{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Page.Machine (
  machineNew ,
  machineDetail ) where

import "fay-base" Data.Text (fromString, pack, Text, (<>))
import "fay-base" Prelude hiding (div, span, id)
import "fay-base" Data.Maybe (whenJust)
import "fay-base" Data.Var (Var, modify)
import "fay-base" FFI (Defined(Defined))

import HaskellReact as HR
import qualified HaskellReact.Bootstrap as B
import qualified HaskellReact.Bootstrap.Button as BTN
import qualified HaskellReact.Tag.Input as II
import qualified HaskellReact.Bootstrap.ButtonDropdown as BD

import qualified Crm.Shared.Machine as M
import qualified Crm.Shared.YearMonthDay as YMD
import qualified Crm.Shared.MachineType as MT
import qualified Crm.Shared.Company as C
import qualified Crm.Shared.UpkeepSequence as US
import qualified HaskellReact.Tag.Hyperlink as A

import qualified Crm.Data.MachineData as MD
import qualified Crm.Data.Data as D
import qualified Crm.Component.DatePicker as DP
import Crm.Component.Editable (editableN)
import Crm.Server (createMachine, updateMachine)
import Crm.Helpers (parseSafely, displayDate, lmap, rmap, formRow', 
  editingInput, eventInt, inputNormalAttrs, formRowCol, formRow, editingTextarea)
import Crm.Router (CrmRouter, navigate, frontPage)

saveButtonRow :: Renderable a
              => a -- ^ label of the button
              -> Fay () -- ^ button on click handler
              -> DOMElement
saveButtonRow buttonLabel clickHandler = 
  div' (class'' ["col-md-9", "col-md-offset-3"]) $
    BTN.button'
      (BTN.buttonProps {
        BTN.bsStyle = Defined "primary" ,
        BTN.onClick = Defined $ const clickHandler })
      buttonLabel

machineDetail :: Bool
              -> Var D.AppState
              -> DP.DatePicker
              -> M.Machine
              -> MT.MachineTypeId
              -> (MT.MachineType, [US.UpkeepSequence])
              -> M.MachineId
              -> YMD.YearMonthDay
              -> (DOMElement, Fay ())
machineDetail editing appVar calendarOpen machine machineTypeId machineTypeTuple machineId nextService = 
  machineDisplay editing button appVar calendarOpen machine machineTypeTuple extraRow
    where
      extraRow = [row "Další servis" (displayDate nextService)]
      setEditing :: Fay ()
      setEditing = modify appVar (\appState -> appState {
        D.navigation = case D.navigation appState of
          D.MachineScreen (MD.MachineData a b c (Left (MD.MachineDetail d e _ f))) ->
            D.MachineScreen (MD.MachineData a b c (Left (MD.MachineDetail d e True f)))
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
  machineDisplay True buttonRow appState datePickerCalendar machine' machineTypeTuple []
    where
      machineTypeEither = case machineTypeId of
        Just(machineTypeId') -> MT.MyInt $ MT.getMachineTypeId machineTypeId'
        Nothing -> MT.MyMachineType machineTypeTuple
      saveNewMachine = createMachine machine' companyId machineTypeEither 
        (navigate (frontPage C.NextService) router)
      buttonRow = saveButtonRow "Vytvoř" saveNewMachine

row :: Renderable a
    => Text -- ^ label of field
    -> a -- ^ the other field
    -> DOMElement
row labelText otherField = 
  div' (class' "form-group") [ 
    label' (class'' ["control-label", "col-md-3"]) (span labelText) , 
    div' (class' "col-md-9") otherField ]

machineDisplay :: Bool -- ^ true editing mode false display mode
               -> DOMElement
               -> Var D.AppState
               -> DP.DatePicker
               -> M.Machine
               -> (MT.MachineType, [US.UpkeepSequence])
               -> [DOMElement]
               -> (DOMElement, Fay ())
machineDisplay editing buttonRow appVar operationStartCalendar
    machine' (machineType, upkeepSequences) extraRow = let

  changeNavigationState :: (MD.MachineData -> MD.MachineData) -> Fay ()
  changeNavigationState fun = modify appVar (\appState -> appState {
    D.navigation = case D.navigation appState of 
      (D.MachineScreen (md @ (MD.MachineData _ _ _ _))) -> D.MachineScreen $ fun md
      _ -> D.navigation appState })

  setMachine :: M.Machine -> Fay ()
  setMachine machine = changeNavigationState (\md -> md { MD.machine = machine })

  row'' labelText value' onChange' = let
    inputAttrs = II.mkInputAttrs {
      II.defaultValue = Defined $ pack value' ,
      II.onChange = Defined onChange' }
    input = editableN inputAttrs inputNormalAttrs editing (text2DOM $ pack value')
    in row labelText input
  row' labelText value' onChange' = row'' labelText value' onChange'
  elements = form' (mkAttrs { className = Defined "form-horizontal" }) $
    B.grid $
      B.row $ [
        formRow'
          "Typ zařízení" 
          (MT.machineTypeName machineType) 
          (const $ return ())
          False
          False ,
        formRow'
          "Výrobce"
          (MT.machineTypeManufacturer machineType)
          (const $ return ())
          False
          False ,
        div' (class' "form-group") [
          label' (class'' ["control-label", "col-md-3"]) (span "Datum uvedení do provozu") ,
          B.col (B.mkColProps 9) $ let
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
              displayedDate setDate ] ,
        row'
          "Úvodní stav motohodin"
          (show $ M.initialMileage machine')
          (let
            setInitialMileage :: Int -> Fay ()
            setInitialMileage int = setMachine $ machine' { M.initialMileage = int }
            in flip whenJust setInitialMileage . parseSafely <=< eventValue ) ,
        formRowCol 
          "Provoz motohodin/rok (Rok má 8760 motohodin)" [
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
            M.note = str } ) <=< eventString) editing False) ] ++ extraRow ++ [
        div' (class' "form-group") buttonRow ]
  in (elements, return ())
