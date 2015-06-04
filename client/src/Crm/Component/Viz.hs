{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Crm.Component.Viz (
  MachineNode (..) ,
  MachineEdge (..) ,
  MachineGraph (..) ,
  interpret ) where

import           FFI
import           Data.Text              (fromString, Text, showInt, (<>), intercalate)
import           Prelude                hiding (intercalate)

import qualified Crm.Shared.MachineType as MT
import qualified Crm.Shared.Machine     as M
import qualified Crm.Router             as R

data MachineNode = MachineNode {
  machineId :: M.MachineId ,
  machine :: M.Machine ,
  machineType :: MT.MachineType }

data MachineEdge = MachineEdge {
  from :: M.MachineId ,
  to :: M.MachineId }

data MachineGraph = MachineGraph [MachineNode] [MachineEdge]

viz :: Text -> Text
viz = ffi " Viz(%1,\"svg\",\"dot\", null) " 

mkId :: M.MachineId -> Text
mkId = ("machine" <>) . showInt . M.getMachineId

mkLabel :: MachineNode -> Text
mkLabel (MachineNode mId m mT) = let
  innerText = (MT.machineTypeName mT) <> " " <> (M.serialNumber m)
  url = R.routeToText $ R.machineDetail mId
  in "[URL=\"" <> url <> "\", label=\"" <> innerText <> "\"]"

-- | Create an svg element from a description of machines and the links between them.
interpret :: MachineGraph -> Text
interpret (MachineGraph nodes edges) = let
  interpretNode (node @ (MachineNode mId _ _)) = let
    in (mkId mId) <> " " <> (mkLabel node) <> ";"
  interpretedNodes = intercalate "" $ interpretNode `map` nodes
  interpretEdge edge = (mkId $ from edge) <> " -> " <> (mkId $ to edge) <> ";"
  interpretedEdges = intercalate "" $ interpretEdge `map` edges
  in viz $ " digraph { " <> interpretedNodes <> interpretedEdges <> " } "
