{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Component.Viz (
  MachineNode (..) ,
  MachineEdge (..) ,
  MachineGraph (..) ,
  interpret ) where

import FFI
import "fay-base" Data.Text (fromString, Text, showInt, (<>), intercalate)
import "fay-base" Prelude hiding (intercalate)

import qualified Crm.Shared.MachineKind as MK
import qualified Crm.Shared.Machine as M

data MachineNode = MachineNode {
  machineId :: M.MachineId ,
  machineKind :: MK.MachineKindEnum ,
  label :: Text }

data MachineEdge = MachineEdge {
  from :: M.MachineId ,
  to :: M.MachineId }

data MachineGraph = MachineGraph [MachineNode] [MachineEdge]

viz :: Text -> Text
viz = ffi " Viz(%1,\"svg\",\"dot\", null) " 

mkId :: M.MachineId -> Text
mkId = ("machine" <>) . showInt . M.getMachineId

mkLabel :: Text -> Text
mkLabel label' = "[label=\"" <> label' <> "\"]"

-- | Create an svg element from a description of machines and the links between them.
interpret :: MachineGraph -> Text
interpret (MachineGraph nodes edges) = let
  interpretNode node = (mkId $ machineId node) <> " " <> (mkLabel $ label node) <> ";"
  interpretedNodes = intercalate "" $ interpretNode `map` nodes
  interpretEdge edge = (mkId $ from edge) <> " -> " <> (mkId $ to edge) <> ";"
  interpretedEdges = intercalate "" $ interpretEdge `map` edges
  in viz $ " digraph { " <> interpretedNodes <> interpretedEdges <> " } "
