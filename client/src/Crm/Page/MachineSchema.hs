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

schema :: (DOMElement, Fay ())
schema = let
  canvas = B.grid $ div' (mkAttrs { id = Defined "graph-canvas" }) ""
  schema' = MachineGraph
    [MachineNode (M.MachineId 1) MK.Compressor "K1", MachineNode (M.MachineId 2) MK.Compressor "K2"]
    [MachineEdge (M.MachineId 1) (M.MachineId 2)]
  graph = interpret schema'
  appendElement = JQ.select "#graph-canvas" >>= JQ.append graph >> return ()
  in (canvas, appendElement)
