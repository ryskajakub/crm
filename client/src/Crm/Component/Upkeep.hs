{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Component.Upkeep (
  upkeepNew
) where

import HaskellReact as HR
import qualified Crm.Shared.Company as C
import qualified Crm.Shared.Machine as M
import qualified Crm.Shared.MachineType as MT
import qualified Crm.Shared.Upkeep as U
import qualified Crm.Shared.UpkeepMachine as UM
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
import qualified HaskellReact.Tag.Hyperlink as A
import Crm.Component.Data
import Crm.Component.Editable (editable)
import Crm.Server (createMachine)

upkeepNew :: MyData
          -> Var AppState
          -> U.Upkeep
          -> [(Int, M.Machine)] -- ^ machine ids -> machines
          -> DOMElement
upkeepNew myData appState upkeep' machines = let
  machineRow (machineId, machine) = 
    B.row [
      let 
        content = span $ pack $ (MT.machineTypeName . M.machineType) machine
        clickHandler = modify appState (\appState' -> case navigation appState' of
          upkeepNew @ (UpkeepNew _ _) -> let 
            oldUpkeepMachines = (U.upkeepMachines upkeep')
            newUpkeepMachines = 
              case find (\(UM.UpkeepMachine _ machineId') -> machineId' == machineId) oldUpkeepMachines of
                Just _ -> 
                  filter (\(UM.UpkeepMachine _ machineId') -> machineId' /= machineId) oldUpkeepMachines
                Nothing -> (UM.newUpkeepMachine machineId) : oldUpkeepMachines
            newUpkeep = upkeep' { U.upkeepMachines = newUpkeepMachines }
            in appState' { navigation = upkeepNew { upkeep = newUpkeep } } )
        link = A.a'' 
          (mkAttrs{onClick = Defined $ const clickHandler})
          (A.mkAAttrs)
          content
        in B.col (B.mkColProps 6) link ,
      B.col (B.mkColProps 6) $ I.textarea I.mkInputProps ]
  submitButton = let 
    newUpkeepHandler = return ()
    buttonProps = BTN.buttonProps {
      BTN.bsStyle = Defined "primary" , 
      BTN.onClick = Defined $ const newUpkeepHandler }
    button = BTN.button' buttonProps [ G.plus , text2DOM " Naplánovat" ]
    in B.col ((B.mkColProps 6){ B.mdOffset = Defined 6 }) button
  dateRow = B.row [
    B.col (B.mkColProps 6) "Datum" ,
    B.col (B.mkColProps 6) $ I.input (I.mkInputProps)]
  in div $ 
    B.grid $ 
      map machineRow machines ++ [dateRow, submitButton]
