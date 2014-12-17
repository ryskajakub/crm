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

upkeepNew :: MyData
          -> Var AppState
          -> U.Upkeep
          -> [M.Machine]
          -> DOMElement
upkeepNew myData appState maintenance machines = let
  machineRow machine = 
    B.row [
      B.col (B.mkColProps 6) $ span $ pack $ (MT.machineTypeName . M.machineType) machine
      , B.col (B.mkColProps 6) $ I.textarea $ I.mkInputProps {
        I.type_ = "textarea" }
    ]
  submitButton = let 
    newUpkeepHandler = return ()
    buttonProps = BTN.buttonProps {
      BTN.bsStyle = Defined "primary" , 
      BTN.onClick = Defined $ const newUpkeepHandler }
    button = BTN.button' buttonProps [ G.plus , text2DOM " Naplánovat" ]
    in B.col ((B.mkColProps 6){ B.mdOffset = Defined 6 }) button
  in div $ 
    B.grid $ 
      map machineRow machines ++ [submitButton] 
