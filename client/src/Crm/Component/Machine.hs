{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Component.Machine (
  machineNew ,
  machineDetail ) where

import HaskellReact as HR
import qualified Crm.Shared.Company as C
import qualified Crm.Shared.Machine as M
import qualified Crm.Shared.YearMonthDay as YMD
import qualified Crm.Shared.MachineType as MT
import "fay-base" Data.Text (fromString, unpack, pack, append, showInt)
import "fay-base" Prelude hiding (div, span, id)
import "fay-base" Data.Maybe (whenJust)
import Data.Var (Var, modify)
import FFI (Defined(Defined))
import HaskellReact.BackboneRouter (link, navigate)
import qualified HaskellReact.Bootstrap as B
import qualified HaskellReact.Bootstrap.Input as I
import qualified HaskellReact.Bootstrap.Button as BTN
import qualified HaskellReact.Bootstrap.Glyphicon as G
import qualified HaskellReact.Tag.Input as II
import qualified HaskellReact.Bootstrap.CalendarInput as CI
import Crm.Component.Data as D
import Crm.Component.Editable (editable, editable', editableN)
import Crm.Server (createMachine, updateMachine)
import Crm.Helpers (parseSafely)

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
              -> MyData
              -> Var AppState
              -> Bool
              -> M.Machine
              -> Int -- id of the machine
              -> DOMElement
machineDetail editing myData appVar calendarOpen machine machineId = machineDisplay
  editing button myData appVar calendarOpen machine
    where
      setEditing :: Fay ()
      setEditing = modify appVar (\appState -> appState {
        navigation = case navigation appState of
          md @ (MachineDetail m _ _ id') -> MachineDetail m False True id'
          _ -> navigation appState })
      editButtonRow =
        div' (class' "col-md-3") $
          BTN.button'
            (BTN.buttonProps { BTN.onClick = Defined $ const setEditing })
            "Jdi do editačního módu"
      editMachineAction = updateMachine (machineId, machine) (return ())
      saveButtonRow' = saveButtonRow "Edituj" editMachineAction
      button = if editing then saveButtonRow' else editButtonRow

machineNew :: MyData
           -> Var AppState
           -> Bool
           -> M.Machine
           -> DOMElement
machineNew myData appState calendarOpen machine' = 
  machineDisplay True buttonRow myData appState calendarOpen machine'
    where
      saveNewMachine = createMachine machine' (\machineId -> do
        modify appState (\appState' -> let
          machines' = machines appState'
          newMachines = machines' ++ [(machineId, machine')]
          in appState' { machines = newMachines })
        navigate "" (router myData))
      buttonRow = saveButtonRow ("Vytvoř") saveNewMachine

machineDisplay :: Bool -- ^ true editing mode false display mode
               -> DOMElement
               -> MyData
               -> Var AppState
               -> Bool
               -> M.Machine
               -> DOMElement
machineDisplay editing buttonRow myData appVar operationStartCalendarOpen' machine' = let
  setMachine :: M.Machine -> Fay ()
  setMachine modifiedMachine = modify appVar (\appState -> appState {
    navigation = case navigation appState of
      mn @ (MachineNew _ _) -> mn { machine = modifiedMachine }
      md @ (MachineDetail _ _ _ _) -> md { machine = modifiedMachine }
      _ -> navigation appState
    })
  setMachineType :: (MT.MachineType -> MT.MachineType) -> Fay ()
  setMachineType modifyMachineType = let
    machineType' = case M.machineType machine' of
      mt @ (MT.MachineType _ _ _) -> modifyMachineType mt
      x -> x
    machine'' = machine' { M.machineType = machineType' }
    in setMachine machine''
  machineType = M.machineType machine'
  inputRow = I.mkInputProps {
    I.labelClassName = Defined "col-md-3"
    , I.wrapperClassName = Defined "col-md-9" }
  row labelText value' onChange' = let
    attrs = class' "form-control"
    inputAttrs = II.mkInputAttrs {
      II.defaultValue = Defined $ pack value' ,
      II.onChange = Defined onChange' }
    input = editableN inputAttrs attrs editing (text2DOM $ pack value')
    inputWrapper = div' (class' "col-md-9") input
    labelColumn = label' (class'' ["control-label", "col-md-3"]) (span labelText)
    in div' (class' "form-group") [ labelColumn , inputWrapper ]
  in form' (mkAttrs { className = Defined "form-horizontal" }) $
    B.grid $
      B.row [
        row
          "Typ zařízení"
          (MT.machineTypeName machineType)
          (eventString >=> (\string -> setMachineType (\mt -> mt { MT.machineTypeName = string }))) ,
        row
          "Výrobce"
          (MT.machineTypeManufacturer machineType)
          (eventString >=> (\string -> setMachineType (\mt -> mt {MT.machineTypeManufacturer = string}))) ,
        row
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
              month | month == "Month" -> setDate YMD.MonthPrecision
              year | year == "Year" -> setDate YMD.YearPrecision
              day | day == "Day" -> setDate YMD.DayPrecision
              _ -> return ()
              where
                setDate precision = setMachine $ machine' {
                  M.machineOperationStartDate = YMD.YearMonthDay year month day precision }
            setPickerOpenness open = modify appVar (\appState -> appState {
              navigation = case navigation appState of
                nm @ (MachineNew _ _) -> nm { operationStartCalendarOpen = open }
                _ -> navigation appState })
            in CI.dayInput editing y m d dayPickHandler operationStartCalendarOpen' setPickerOpenness ] ,
        row
          "Úvodní stav motohodin"
          (unpack $ showInt $ M.initialMileage machine')
          (let
            setInitialMileage :: Int -> Fay ()
            setInitialMileage int = setMachine $ machine' { M.initialMileage = int }
            in flip whenJust setInitialMileage . parseSafely <=< eventValue ) ,
        row
          "Provoz (motohodin/rok)"
          (unpack $ showInt $ M.mileagePerYear machine')
          (let
            setMileagePerYear :: Int -> Fay ()
            setMileagePerYear int = setMachine $ machine' { M.mileagePerYear = int }
            in flip whenJust setMileagePerYear . parseSafely <=< eventValue) ,
        div' (class' "form-group") buttonRow ]
