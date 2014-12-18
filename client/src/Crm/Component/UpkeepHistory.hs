{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Component.UpkeepHistory (
  upkeepHistory
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
import Crm.Server (createMachine, createUpkeep)

import Debug.Trace

upkeepHistory :: [(Int, U.Upkeep)]
              -> DOMElement
upkeepHistory upkeeps = let
  upkeepHtml (upkeepId, upkeep) = let
    upkeepDate = B.row $ (B.col $ B.mkColProps 3) $ pack $ U.upkeepDate upkeep
    in upkeepDate
  in div [
    h2 "Historie servisů" ,
    reactInstance2DOM $ B.grid $ map upkeepHtml upkeeps ]
