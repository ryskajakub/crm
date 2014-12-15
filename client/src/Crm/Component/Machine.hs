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

machineNew :: MyData
           -> Var AppState
           -> M.Machine
           -> DOMElement
machineNew myData appVar machine' =
  let
    saveNewMachine = createMachine machine'
    setMachine modifiedMachine = modify appVar (\appState -> appState {
      navigation = case navigation appState of
        nm @ (MachineNew _) -> nm { machine = modifiedMachine }
        _ -> navigation appState
      })
    machineType = M.machineType machine'
    setMachineTypeName event = do
      value <- eventValue event
      let
        modifiedMachine = machine' { M.machineType = let
          modifiedMachineType = case machineType of
            mt @ (MT.MachineType _ _) -> mt {
              MT.machineTypeName = unpack $ value }
            x -> x
          in modifiedMachineType }
        in setMachine modifiedMachine
    setMachineTypeManufacturer event = do
      value <- eventValue event
      let
        modifiedMachine = machine' { M.machineType = let
          newMachineType = case machineType of
            mt @ (MT.MachineType _ _) -> mt {
              MT.machineTypeManufacturer = unpack $ value }
            x -> x
          in newMachineType }
        in setMachine modifiedMachine
    setOperationStartDate event = do
      value <- eventValue event
      let
        newMachine = machine' {
          M.machineOperationStartDate = unpack $ value }
        in setMachine newMachine
    inputRow = I.mkInputProps {
      I.labelClassName = Defined "col-md-3"
      , I.wrapperClassName = Defined "col-md-9" }
    in form' (mkAttrs { className = Defined "form-horizontal" }) $
      B.grid $
        B.row [
          I.input $ inputRow {
            I.label_ = Defined "Typ zařízení"
            , I.onChange = Defined setMachineTypeName }
          , I.input $ inputRow {
            I.label_ = Defined "Výrobce"
            , I.onChange = Defined setMachineTypeManufacturer }
          , I.input $ inputRow {
            I.label_ = Defined "Datum uvedení do provozu"
            , I.onChange = Defined setOperationStartDate }
          , div' (class' "form-group") $
              div' (class'' ["col-md-9", "col-md-offset-3"]) $
                BTN.button'
                  (BTN.buttonProps {
                    BTN.bsStyle = Defined "primary"
                    , BTN.onClick = Defined const $ saveNewMachine })
                  "Přidej"
        ]
