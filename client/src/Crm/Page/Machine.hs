{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Page.Machine (
  machineNew ,
  machineTypePhase1Form ,
  machineDetail ) where

import "fay-base" Data.Text (fromString, unpack, pack, showInt, Text)
import "fay-base" Prelude hiding (div, span, id)
import "fay-base" Data.Maybe (whenJust, isNothing)
import "fay-base" Data.Var (Var, modify)
import "fay-base" FFI (Defined(Defined))

import HaskellReact as HR
import qualified HaskellReact.Bootstrap as B
import qualified HaskellReact.Bootstrap.Button as BTN
import qualified HaskellReact.Tag.Input as II

import qualified Crm.Shared.Machine as M
import qualified Crm.Shared.YearMonthDay as YMD
import qualified Crm.Shared.MachineType as MT
import qualified Crm.Shared.Company as C
import qualified Crm.Shared.UpkeepSequence as US

import qualified Crm.Data as D
import qualified Crm.Component.DatePicker as DP
import Crm.Component.Editable (editableN)
import Crm.Server (createMachine, updateMachine, fetchMachineType, fetchMachineTypesAutocomplete)
import Crm.Helpers (parseSafely, displayDate, lmap, rmap, formRow, formRow')
import Crm.Router (CrmRouter, navigate, frontPage, newMachinePhase2)
import Crm.Component.Autocomplete (autocompleteInput)

machineTypePhase1Form :: Maybe MT.MachineTypeId
                      -> (MT.MachineType, [US.UpkeepSequence])
                      -> Var D.AppState
                      -> CrmRouter
                      -> C.CompanyId
                      -> (DOMElement, Fay ())
machineTypePhase1Form machineTypeId (machineType, upkeepSequences) appVar crmRouter companyId = let

  setMachineType :: MT.MachineType -> Fay ()
  setMachineType modifiedMachineType = 
    D.modifyState appVar (\navig -> navig { D.machineTypeTuple = (modifiedMachineType, upkeepSequences) })

  setMachineTypeId :: Maybe MT.MachineTypeId -> Fay ()
  setMachineTypeId machineTypeId' = 
    D.modifyState appVar (\navig -> navig { D.maybeMachineTypeId = machineTypeId' })

  (machineTypeInput, afterRenderCallback) =
    autocompleteInput 
      inputNormalAttrs
      (\text -> do
        setMachineType (machineType { MT.machineTypeName = unpack text })
        setMachineTypeId Nothing)
      (\text -> if text /= "" 
        then fetchMachineType text (\maybeTuple -> case maybeTuple of
          Just (machineTypeId', machineType') -> do
            setMachineType machineType'
            setMachineTypeId $ Just machineTypeId'
          Nothing -> return () )
        else return () )
      fetchMachineTypesAutocomplete
      "machine-type-autocomplete"
      (II.mkInputAttrs {
        II.defaultValue = Defined $ pack $ MT.machineTypeName machineType })
  submitButtonHandler = do
    modify appVar (\appState -> appState {
      D.machineTypeFromPhase1 = (machineType, upkeepSequences) ,
      D.maybeMachineIdFromPhase1 = machineTypeId })
    navigate (newMachinePhase2 companyId) crmRouter
    
  result = form' (mkAttrs { className = Defined "form-horizontal" }) $
    B.grid $
      B.row $ [
        formRow
          "Typ zařízení"
          machineTypeInput ,
        formRow'
          "Výrobce"
          (MT.machineTypeManufacturer machineType)
          (eventString >=> (\string -> setMachineType (machineType { MT.machineTypeManufacturer = string })))
          (isNothing machineTypeId) ,
        formRow'
          "Interval servisu"
          (unpack $ showInt $ MT.upkeepPerMileage machineType)
          (eventValue >=> (\str -> case parseSafely str of
            Just(int) -> setMachineType (machineType { MT.upkeepPerMileage = int })
            Nothing -> return ())) 
          (isNothing machineTypeId) ,
        saveButtonRow "Dále" submitButtonHandler ]
  in (result, afterRenderCallback)

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
              -> DP.DatePicker
              -> M.Machine
              -> MT.MachineTypeId
              -> MT.MachineType
              -> M.MachineId
              -> YMD.YearMonthDay
              -> (DOMElement, Fay ())
machineDetail editing appVar calendarOpen machine machineTypeId machineType machineId nextService = 
  machineDisplay editing button appVar calendarOpen machine (machineType,[]) (Just machineTypeId) extraRow
    where
      extraRow = [row "Další servis" (displayDate nextService)]
      setEditing :: Fay ()
      setEditing = modify appVar (\appState -> appState {
        D.navigation = case D.navigation appState of
          D.MachineDetail m mt mtid c _ id' ns -> D.MachineDetail m mt mtid c True id' ns
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
  machineDisplay True buttonRow appState datePickerCalendar machine' machineTypeTuple machineTypeId []
    where
      machineTypeEither = case machineTypeId of
        Just(machineTypeId') -> MT.MyInt $ MT.getMachineTypeId machineTypeId'
        Nothing -> MT.MyMachineType machineTypeTuple
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

inputNormalAttrs :: Attributes
inputNormalAttrs = class' "form-control"

machineDisplay :: Bool -- ^ true editing mode false display mode
               -> DOMElement
               -> Var D.AppState
               -> DP.DatePicker
               -> M.Machine
               -> (MT.MachineType, [US.UpkeepSequence])
               -> Maybe MT.MachineTypeId -- ^ machine type id
               -> [DOMElement]
               -> (DOMElement, Fay ())
machineDisplay editing buttonRow appVar operationStartCalendar
    machine' (machineTypeTuple @ (machineType, upkeepSequences)) machineTypeId extraRow = let

  setMachine :: M.Machine -> Fay ()
  setMachine modifiedMachine = modify appVar (\appState -> appState {
    D.navigation = case D.navigation appState of
      mn @ (D.MachineNew _ _ _ _ _) -> mn { D.machine = modifiedMachine }
      md @ (D.MachineDetail _ _ _ _ _ _ _) -> md { D.machine = modifiedMachine }
      _ -> D.navigation appState })

  row'' labelText value' onChange' editing' = let
    inputAttrs = II.mkInputAttrs {
      II.defaultValue = Defined $ pack value' ,
      II.onChange = Defined onChange' }
    input = editableN inputAttrs inputNormalAttrs editing' (text2DOM $ pack value')
    in row labelText input
  row' labelText value' onChange' = row'' labelText value' onChange' editing
  changeNavigationState :: (D.NavigationState -> D.NavigationState) -> Fay ()
  changeNavigationState fun = modify appVar (\appState -> appState {
    D.navigation = case D.navigation appState of 
      mn @ (D.MachineNew _ _ _ _ _) -> fun mn 
      md @ (D.MachineDetail _ _ _ _ _ _ _) -> fun md
      _ -> D.navigation appState })
  elements = form' (mkAttrs { className = Defined "form-horizontal" }) $
    B.grid $
      B.row $ [
        formRow'
          "Typ zařízení" 
          (MT.machineTypeName machineType) 
          undefined
          False ,
        formRow'
          "Výrobce"
          (MT.machineTypeManufacturer machineType)
          undefined
          False ,
        div' (class' "form-group") [
          label' (class'' ["control-label", "col-md-3"]) (span "Datum uvedení do provozu") ,
          B.col (B.mkColProps 9) $ let
            setDatePickerDate date = changeNavigationState (\state ->
              state { D.operationStartCalendar = 
                lmap (const date) (D.operationStartCalendar state) })
            setPickerOpenness openness = changeNavigationState (\state ->
              state { D.operationStartCalendar = 
                rmap (const openness) (D.operationStartCalendar state) })
            displayedDate = M.machineOperationStartDate machine'
            setDate date = let
              newMachine = machine' { M.machineOperationStartDate = date }
              in setMachine newMachine
            in DP.datePicker editing operationStartCalendar setDatePickerDate setPickerOpenness
              displayedDate setDate ]  ,
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
  in (elements, return ())
