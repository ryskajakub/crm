{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Page.Machine (
  machineNew ,
  machineDetail ) where

import "fay-base" Data.Text (fromString, unpack, pack, showInt, Text)
import "fay-base" Prelude hiding (div, span, id)
import "fay-base" Data.Maybe (whenJust, isNothing)
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
import qualified Crm.Shared.Company as C
import qualified Crm.Data as D
import Crm.Component.Editable (editableN)
import Crm.Server (createMachine, updateMachine, fetchMachineType)
import Crm.Helpers (parseSafely, displayDate)
import Crm.Router (CrmRouter, navigate, frontPage)
import Crm.Component.Autocomplete (autocompleteInput)

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
              -> Var D.AppState
              -> Bool
              -> M.Machine
              -> MT.MachineTypeId
              -> MT.MachineType
              -> M.MachineId
              -> YMD.YearMonthDay
              -> (DOMElement, Fay ())
machineDetail editing appVar calendarOpen machine machineTypeId machineType machineId nextService = 
  machineDisplay editing button appVar calendarOpen machine machineType (Just machineTypeId) extraRow
    where
      extraRow = [row "Další servis" (displayDate nextService)]
      setEditing :: Fay ()
      setEditing = modify appVar (\appState -> appState {
        D.navigation = case D.navigation appState of
          D.MachineDetail m mt mtid _ _ id' ns -> D.MachineDetail m mt mtid False True id' ns
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
           -> Bool
           -> M.Machine
           -> C.CompanyId
           -> MT.MachineType
           -> Maybe MT.MachineTypeId
           -> (DOMElement, Fay ())
machineNew router appState calendarOpen machine' companyId machineType machineTypeId = 
  machineDisplay True buttonRow appState calendarOpen machine' machineType machineTypeId []
    where
      machineTypeEither = case machineTypeId of
        Just(machineTypeId') -> MT.MyInt $ MT.getMachineTypeId machineTypeId'
        Nothing -> MT.MyMachineType machineType
      saveNewMachine = createMachine machine' companyId machineTypeEither (navigate frontPage router)
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
               -> Bool
               -> M.Machine
               -> MT.MachineType
               -> Maybe MT.MachineTypeId -- ^ machine type id
               -> [DOMElement]
               -> (DOMElement, Fay ())
machineDisplay editing buttonRow appVar operationStartCalendarOpen' 
    machine' machineType machineTypeId extraRow = let
  setMachine :: M.Machine -> Fay ()
  setMachine modifiedMachine = modify appVar (\appState -> appState {
    D.navigation = case D.navigation appState of
      mn @ (D.MachineNew _ _ _ _ _) -> mn { D.machine = modifiedMachine }
      md @ (D.MachineDetail _ _ _ _ _ _ _) -> md { D.machine = modifiedMachine }
      _ -> D.navigation appState })
  setMachineType :: MT.MachineType -> Fay ()
  setMachineType modifiedMachineType = modify appVar (\appState -> appState {
    D.navigation = case D.navigation appState of
      mn @ (D.MachineNew _ _ _ _ _) -> mn { D.machineType = modifiedMachineType }
      md @ (D.MachineDetail _ _ _ _ _ _ _) -> md { D.machineType = modifiedMachineType }
      _ -> D.navigation appState })
  setMachineTypeId :: MT.MachineTypeId -> Fay ()
  setMachineTypeId machineTypeId' = modify appVar (\appState -> appState {
    D.navigation = case D.navigation appState of
      mn @ (D.MachineNew _ _ _ _ _) -> mn { D.maybeMachineTypeId = Just machineTypeId' }
      md @ (D.MachineDetail _ _ _ _ _ _ _) -> md { D.machineTypeId = machineTypeId' }
      _ -> D.navigation appState })
  unsetMachineTypeId :: Fay ()
  unsetMachineTypeId = modify appVar (\appState -> appState {
    D.navigation = case D.navigation appState of
      mn @ (D.MachineNew _ _ _ _ _) -> mn { D.maybeMachineTypeId = Nothing }
      _ -> D.navigation appState })
  row'' labelText value' onChange' editing' = let
    inputAttrs = II.mkInputAttrs {
      II.defaultValue = Defined $ pack value' ,
      II.onChange = Defined onChange' }
    input = editableN inputAttrs inputNormalAttrs editing' (text2DOM $ pack value')
    in row labelText input
  inputNormalAttrs = class' "form-control"
  row' labelText value' onChange' = row'' labelText value' onChange' editing
  (machineTypeInput, afterRenderCallback) = 
    autocompleteInput 
      inputNormalAttrs
      (\text -> case text of 
        text' | text' /= "" -> fetchMachineType text (\maybeTuple -> case maybeTuple of
          Just (machineTypeId', machineType') -> do
            setMachineType machineType'
            setMachineTypeId machineTypeId'
          Nothing -> unsetMachineTypeId )
        _ -> return () )
      "machine-type-autocomplete"
      (II.mkInputAttrs {
        II.defaultValue = Defined $ pack $ MT.machineTypeName machineType })
  elements = form' (mkAttrs { className = Defined "form-horizontal" }) $
    B.grid $
      B.row $ [
        row
          "Typ zařízení" 
          (if editing then machineTypeInput else (text2DOM $ pack $ MT.machineTypeName machineType)),
        row''
          "Výrobce"
          (MT.machineTypeManufacturer machineType)
          (eventString >=> (\string -> setMachineType (machineType { MT.machineTypeManufacturer = string })))
          (isNothing machineTypeId) ,
        row''
          "Interval servisu"
          (unpack $ showInt $ MT.upkeepPerMileage machineType)
          (eventValue >=> (\str -> case parseSafely str of
            Just(int) -> setMachineType (machineType { MT.upkeepPerMileage = int })
            Nothing -> return ())) 
          (isNothing machineTypeId) ,
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
                nm @ (D.MachineNew _ _ _ _ _) -> nm { D.operationStartCalendarOpen = open }
                _ -> D.navigation appState })
            changeViewHandler changeViewCommand = let
              (newYear, newMonth) = case changeViewCommand of
                CI.PreviousYear           -> (y - 1, m)
                CI.PreviousMonth | m == 1 -> (y - 1, 12)
                CI.PreviousMonth          -> (y, m - 1)
                CI.NextMonth | m == 12    -> (y + 1, 1)
                CI.NextMonth              -> (y, m + 1)
                CI.NextYear               -> (y + 1, m)
              newDate = YMD.YearMonthDay newYear newMonth d YMD.DayPrecision
              in setMachine $ machine' {
                M.machineOperationStartDate = newDate }
            in CI.dayInput editing y m d dayPickHandler 
              operationStartCalendarOpen' setPickerOpenness changeViewHandler ] ,
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
  in (elements, afterRenderCallback)
