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

import qualified JQuery as JQ

import Crm.Component.Viz (viz)

schema :: (DOMElement, Fay ())
schema = let
  canvas = div' (mkAttrs { id = Defined "graph-canvas" }) ""
  graph = viz " digraph { main -> init } "
  appendElement = JQ.select "#graph-canvas" >>= JQ.append graph >> return ()
  in (canvas, appendElement)
