{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Component.UpkeepHistory (
  upkeepHistory ) where

import "fay-base" Data.Text (fromString, unpack, pack, append, showInt)
import "fay-base" Prelude hiding (div, span, id)
import Data.Var (Var, modify)
import FFI (Defined(Defined))

import HaskellReact as HR
import HaskellReact.BackboneRouter (link, navigate)
import qualified HaskellReact.Bootstrap as B
import qualified HaskellReact.Bootstrap.Input as I
import qualified HaskellReact.Bootstrap.Button as BTN
import qualified HaskellReact.Bootstrap.Glyphicon as G
import qualified HaskellReact.Tag.Input as II
import qualified HaskellReact.Tag.Hyperlink as A

import qualified Crm.Shared.Company as C
import qualified Crm.Shared.Machine as M
import qualified Crm.Shared.MachineType as MT
import qualified Crm.Shared.Upkeep as U
import qualified Crm.Shared.UpkeepMachine as UM
import Crm.Component.Data
import Crm.Component.Editable (editable)
import Crm.Server (createMachine, createUpkeep)
import Crm.Helpers

upkeepHistory :: [(Int, U.Upkeep)]
              -> DOMElement
upkeepHistory upkeeps = let
  upkeepHtml (upkeepId, upkeep) = let
    upkeepMachines = U.upkeepMachines upkeep
    upkeepDate = B.col (B.mkColProps 12) (displayDate $ U.upkeepDate upkeep)
    upkeepMachineHtml upkeepMachine = B.col
      (B.mkColProps 3)
      (pack $ UM.upkeepMachineNote upkeepMachine)
    row = B.row (upkeepDate : map upkeepMachineHtml upkeepMachines)
    in row
  in div [
    h2 "Historie servisů" ,
    B.grid $ map upkeepHtml upkeeps ]
