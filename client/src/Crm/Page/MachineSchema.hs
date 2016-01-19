{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Crm.Page.MachineSchema ( schema ) where

import           Data.Text              (fromString)
import           Prelude                hiding (div, span, id)
import           FFI                    (Defined(Defined))
import           Data.Maybe             (onJust, mapMaybe)

import           HaskellReact           as HR
import qualified HaskellReact.Bootstrap as B
import qualified JQuery                 as JQ

import qualified Crm.Shared.Machine     as M
import qualified Crm.Shared.MachineType as MT

import           Crm.Component.Viz
import           Crm.Helpers

schema :: 
  [(M.MachineId, M.Machine, MT.MachineType, Maybe M.MachineId)] -> 
  (DOMElement, Fay ())
schema machines = (page, appendElement) where

  page = B.grid $ B.row $ pageInfo' ++ [canvas] where
    pageInfo' = pageInfo "Schéma zapojení strojů" $ Just "Schéma, jak jsou stroje zapojené. Šipka je po proudu vzduchu."
    canvas = B.col (B.mkColProps 12) $ div' (mkAttrs { id = Defined "graph-canvas" }) ""

  mkNodes = map $ \(machineId', machine', machineType', _) ->
    MachineNode machineId' machine' machineType'
  mkEdges = mapMaybe $ \(fromMachineId, _, _, toMachineId) ->
    (\toMachineId' -> MachineEdge fromMachineId toMachineId') `onJust` toMachineId
  graph = interpret $ MachineGraph (mkNodes machines) (mkEdges machines)
  appendElement = JQ.select "#graph-canvas" >>= JQ.append graph >> return ()
