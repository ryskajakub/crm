{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Page.UpkeepHistory (
  upkeepHistory ) where

import "fay-base" Data.Text (fromString, pack)
import "fay-base" Prelude hiding (div, span, id)

import HaskellReact as HR
import qualified HaskellReact.Bootstrap as B

import qualified Crm.Shared.Upkeep as U
import qualified Crm.Shared.UpkeepMachine as UM
import Crm.Helpers (displayDate)

upkeepHistory :: [(U.Upkeep,[UM.UpkeepMachine'])]
              -> DOMElement
upkeepHistory upkeeps = let
  upkeepHtml (upkeep, upkeepMachines) = let
    upkeepDate = B.col (B.mkColProps 12) (displayDate $ U.upkeepDate upkeep)
    upkeepMachineHtml upkeepMachine = B.col
      (B.mkColProps 3)
      (pack $ UM.upkeepMachineNote $ fst upkeepMachine)
    row = B.row (upkeepDate : map upkeepMachineHtml upkeepMachines)
    in row
  in div [
    h2 "Historie servisů" ,
    B.grid $ map upkeepHtml upkeeps ]
