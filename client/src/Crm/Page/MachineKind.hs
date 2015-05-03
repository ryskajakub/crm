{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Page.MachineKind (
  compressorExtraRows ,
  dryerExtraRows) where

import "fay-base" Prelude hiding (div, span, id)
import "fay-base" Data.Text (fromString)

import HaskellReact

import qualified Crm.Shared.Compressor as MC

import Crm.Component.Form (row')

compressorExtraRows :: Bool
                    -> MC.Compressor
                    -> (MC.Compressor -> Fay ())
                    -> [DOMElement]
compressorExtraRows editing compressor setCompressor = [
  row'
    editing
    "Poznámka ke kompresoru"
    (MC.note compressor)
    (eventString >=> (\s -> setCompressor $ compressor { MC.note = s })) ]


dryerExtraRows :: [DOMElement]
dryerExtraRows = []
