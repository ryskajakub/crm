{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Page.MachineSchema ( schema ) where

import "fay-base" Data.Text (fromString)
import "fay-base" Prelude hiding (div, span, id)
import "fay-base" FFI (Defined(Defined))
import "fay-base" Data.Maybe (onJust, mapMaybe)

import HaskellReact as HR
import qualified HaskellReact.Bootstrap as B

import qualified JQuery as JQ

import Crm.Component.Viz
import Crm.Helpers

import qualified Crm.Shared.Machine as M
import qualified Crm.Shared.MachineType as MT

schema :: [(M.MachineId, M.Machine, MT.MachineType, Maybe M.MachineId)]
       -> (DOMElement, Fay ())
schema machines = let

  pageInfo' = pageInfo "Schéma zapojení strojů" $ Just "Schéma, jak jsou stroje zapojené. Šipka je po proudu vzduchu."
  canvas = B.col (B.mkColProps 12) $ div' (mkAttrs { id = Defined "graph-canvas" }) ""
  page = B.grid $ B.row $ pageInfo' ++ [canvas]

  mkNodes = map $ \(machineId', machine', machineType', _) ->
    MachineNode machineId' machine' machineType'
  mkEdges = mapMaybe $ \(fromMachineId, _, _, toMachineId) ->
    (\toMachineId' -> MachineEdge fromMachineId toMachineId') `onJust` toMachineId
  graph = interpret $ MachineGraph (mkNodes machines) (mkEdges machines)
  appendElement = JQ.select "#graph-canvas" >>= JQ.append graph >> return ()
  in (page, appendElement)
