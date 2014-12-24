{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Component.Machine (
  machineNew ,
  machineDetail ) where

import "fay-base" Data.Text (fromString, unpack, pack, showInt, Text)
import "fay-base" Prelude hiding (div, span, id)
import "fay-base" Data.Maybe (whenJust)
import Data.Var (Var, modify)
import "fay-base" FFI (Defined(Defined))

import HaskellReact as HR
import qualified HaskellReact.Bootstrap as B
import qualified HaskellReact.Bootstrap.Button as BTN
import qualified HaskellReact.Tag.Input as II
import qualified HaskellReact.Bootstrap.CalendarInput as CI

import qualified Crm.Shared.Machine as M
import qualified Crm.Shared.YearMonthDay as YMD
import qualified Crm.Shared.MachineType as MT
import qualified Crm.Data as D
import Crm.Component.Editable (editableN)
import Crm.Server (createMachine, updateMachine)
import Crm.Helpers (parseSafely, displayDate)
import Crm.Router (CrmRouter, navigate, frontPage)

saveButtonRow :: Renderable a
              => a -- ^ label of the button
              -> Fay () -- ^ button on click handler
              -> DOMElement
saveButtonRow label clickHandler = 
  div' (class'' ["col-md-9", "col-md-offset-3"]) $
    BTN.button'
      (BTN.buttonProps {
        BTN.bsStyle = Defined "primary" ,
        BTN.onClick = Defined $ const clickHandler })
      label

machineDetail :: Bool
              -> CrmRouter
              -> Var D.AppState
              -> Bool
              -> M.Machine
              -> Int -- id of the machine
              -> YMD.YearMonthDay
              -> DOMElement
machineDetail editing router appVar calendarOpen machine machineId nextService = 
  machineDisplay editing button router appVar calendarOpen machine extraRow
    where
      extraRow = [row "Další servis" (displayDate nextService)]
      setEditing :: Fay ()
      setEditing = modify appVar (\appState -> appState {
        D.navigation = case D.navigation appState of
          D.MachineDetail m _ _ id' ns -> D.MachineDetail m False True id' ns
          _ -> D.navigation appState })
      editButtonRow =
        div' (class' "col-md-3") $
          BTN.button'
            (BTN.buttonProps { BTN.onClick = Defined $ const setEditing })
            "Jdi do editačního módu"
      editMachineAction = updateMachine (machineId, machine) (return ())
      saveButtonRow' = saveButtonRow "Edituj" editMachineAction
      button = if editing then saveButtonRow' else editButtonRow

machineNew :: CrmRouter
           -> Var D.AppState
           -> Bool
           -> M.Machine
           -> DOMElement
machineNew router appState calendarOpen machine' = 
  machineDisplay True buttonRow router appState calendarOpen machine' []
    where
      saveNewMachine = createMachine machine' (\machineId -> do
        modify appState (\appState' -> let
          machines' = D.machines appState'
          newMachines = machines' ++ [(machineId, machine')]
          in appState' { D.machines = newMachines })
        navigate frontPage router )
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
               -> CrmRouter
               -> Var D.AppState
               -> Bool
               -> M.Machine
               -> [DOMElement]
               -> DOMElement
machineDisplay editing buttonRow _ appVar operationStartCalendarOpen' machine' extraRow = let
  setMachine :: M.Machine -> Fay ()
  setMachine modifiedMachine = modify appVar (\appState -> appState {
    D.navigation = case D.navigation appState of
      mn @ (D.MachineNew _ _) -> mn { D.machine = modifiedMachine }
      md @ (D.MachineDetail _ _ _ _ _) -> md { D.machine = modifiedMachine }
      _ -> D.navigation appState })
  setMachineType :: (MT.MachineType -> MT.MachineType) -> Fay ()
  setMachineType modifyMachineType = let
    machineType' = case M.machineType machine' of
      mt @ (MT.MachineType _ _ _) -> modifyMachineType mt
      x -> x
    machine'' = machine' { M.machineType = machineType' }
    in setMachine machine''
  machineType = M.machineType machine'
  row' labelText value' onChange' = let
    attrs = class' "form-control"
    inputAttrs = II.mkInputAttrs {
      II.defaultValue = Defined $ pack value' ,
      II.onChange = Defined onChange' }
    input = editableN inputAttrs attrs editing (text2DOM $ pack value')
    in row labelText input
  in form' (mkAttrs { className = Defined "form-horizontal" }) $
    B.grid $
      B.row $ [
        row'
          "Typ zařízení"
          (MT.machineTypeName machineType)
          (eventString >=> (\string -> setMachineType (\mt -> mt { MT.machineTypeName = string }))) ,
        row'
          "Výrobce"
          (MT.machineTypeManufacturer machineType)
          (eventString >=> (\string -> setMachineType (\mt -> mt {MT.machineTypeManufacturer = string}))) ,
        row'
          "Interval servisu"
          (unpack $ showInt $ MT.upkeepPerMileage machineType)
          (eventValue >=> (\str -> case parseSafely str of
            Just(int) -> setMachineType (\mt -> mt {MT.upkeepPerMileage = int })
            Nothing -> return ())) ,
        div' (class' "form-group") [
          label' (class'' ["control-label", "col-md-3"]) (span "Datum uvedení do provozu") ,
          B.col (B.mkColProps 9) $ let
            YMD.YearMonthDay y m d _ = M.machineOperationStartDate machine'
            dayPickHandler year month day precision = case precision of
              month' | month' == "Month" -> setDate YMD.MonthPrecision
              year' | year' == "Year" -> setDate YMD.YearPrecision
              day' | day' == "Day" -> setDate YMD.DayPrecision
              _ -> return ()
              where
                setDate precision' = setMachine $ machine' {
                  M.machineOperationStartDate = YMD.YearMonthDay year month day precision' }
            setPickerOpenness open = modify appVar (\appState -> appState {
              D.navigation = case D.navigation appState of
                nm @ (D.MachineNew _ _) -> nm { D.operationStartCalendarOpen = open }
                _ -> D.navigation appState })
            in CI.dayInput editing y m d dayPickHandler operationStartCalendarOpen' setPickerOpenness ] ,
        row'
          "Úvodní stav motohodin"
          (unpack $ showInt $ M.initialMileage machine')
          (let
            setInitialMileage :: Int -> Fay ()
            setInitialMileage int = setMachine $ machine' { M.initialMileage = int }
            in flip whenJust setInitialMileage . parseSafely <=< eventValue ) ,
        row'
          "Provoz (motohodin/rok)"
          (unpack $ showInt $ M.mileagePerYear machine')
          (let
            setMileagePerYear :: Int -> Fay ()
            setMileagePerYear int = setMachine $ machine' { M.mileagePerYear = int }
            in flip whenJust setMileagePerYear . parseSafely <=< eventValue)
        ] ++ extraRow ++ [
        div' (class' "form-group") buttonRow ]
