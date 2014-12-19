{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Component.Machine (
  machineNew
) where

import HaskellReact as HR
import qualified Crm.Shared.Company as C
import qualified Crm.Shared.Machine as M
import qualified Crm.Shared.MachineType as MT
import "fay-base" Data.Text (fromString, unpack, pack, append, showInt)
import "fay-base" Prelude hiding (div, span, id)
import Data.Var (Var, modify)
import FFI (Defined(Defined))
import HaskellReact.BackboneRouter (link, navigate)
import qualified HaskellReact.Bootstrap as B
import qualified HaskellReact.Bootstrap.Input as I
import qualified HaskellReact.Bootstrap.Button as BTN
import qualified HaskellReact.Bootstrap.Glyphicon as G
import qualified HaskellReact.Tag.Input as II
import Crm.Component.Data
import Crm.Component.Editable (editable)
import Crm.Server (createMachine)
import Crm.Helpers (parseSafely)

machineNew :: MyData
           -> Var AppState
           -> M.Machine
           -> DOMElement
machineNew myData appVar machine' = let
  saveNewMachine = createMachine machine' (\machineId -> do
    modify appVar (\appState -> let
      machines' = machines appState
      newMachines = machines' ++ [(machineId, machine')]
      in appState { machines = newMachines })
    navigate "" (router myData))
  setMachine :: M.Machine -> Fay ()
  setMachine modifiedMachine = modify appVar (\appState -> appState {
    navigation = case navigation appState of
      nm @ (MachineNew _) -> nm { machine = modifiedMachine }
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
  in form' (mkAttrs { className = Defined "form-horizontal" }) $
    B.grid $
      B.row [
        I.input $ inputRow {
          I.label_ = Defined "Typ zařízení" ,
          I.onChange = Defined $ eventString >=>
            (\string -> setMachineType (\mt -> mt { MT.machineTypeName = string }))} ,
        I.input $ inputRow {
          I.label_ = Defined "Výrobce" ,
          I.onChange = Defined $ eventString >=>
            (\string -> setMachineType (\mt -> mt {MT.machineTypeManufacturer = string}))} ,
        I.input $ inputRow {
          I.label_ = Defined "Interval servisu" ,
          I.onChange = Defined $ eventValue >=> (\str -> case parseSafely str of
            Just(int) -> setMachineType (\mt -> mt {MT.upkeepPerMileage = int })
            Nothing -> return ())} ,
        I.input $ inputRow {
          I.label_ = Defined "Datum uvedení do provozu" ,
          I.onChange = Defined $ eventString >=> 
            (\string -> setMachine $ machine' { M.machineOperationStartDate = string } )} ,
        div' (class' "form-group") $
          div' (class'' ["col-md-9", "col-md-offset-3"]) $
            BTN.button'
              (BTN.buttonProps {
                BTN.bsStyle = Defined "primary"
                , BTN.onClick = Defined $ const saveNewMachine })
                "Přidej"
      ]
