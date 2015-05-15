{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Page.MachineSchema ( schema ) where

import "fay-base" Data.Text (fromString, pack, (<>), unpack, Text, showInt)
import "fay-base" Prelude hiding (div, span, id)
import "fay-base" Data.Var (Var, modify)
import "fay-base" FFI (Defined(Defined))

import HaskellReact as HR
import qualified HaskellReact.Bootstrap as B

import qualified JQuery as JQ

import Crm.Component.Viz

import qualified Crm.Shared.Machine as M
import qualified Crm.Shared.MachineKind as MK
import qualified Crm.Shared.MachineType as MT

schema :: [(M.MachineId, M.Machine, MT.MachineType)]
       -> (DOMElement, Fay ())
schema machines = let
  canvas = B.grid $ div' (mkAttrs { id = Defined "graph-canvas" }) ""
  mkNodes = map $ \(machineId, machine, machineType) -> let
    labelForMachine = (pack $ MT.machineTypeName machineType) <> " " <> (pack $ M.serialNumber machine)
    in MachineNode machineId (MT.kind machineType) labelForMachine 
  graph = interpret $ MachineGraph (mkNodes machines) []
  appendElement = JQ.select "#graph-canvas" >>= JQ.append graph >> return ()
  in (canvas, appendElement)
