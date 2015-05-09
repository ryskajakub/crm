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
import qualified Crm.Shared.Dryer as MD

import Crm.Component.Form

compressorExtraRows :: Bool
                    -> MC.Compressor
                    -> (MC.Compressor -> Fay ())
                    -> [DOMElement]
compressorExtraRows editing compressor setCompressor = [
  row'
    editing
    "Poznámka ke kompresoru"
    (SetValue $ MC.note compressor)
    (eventString >=> (\s -> setCompressor $ compressor { MC.note = s })) ]


dryerExtraRows :: Bool
               -> MD.Dryer
               -> (MD.Dryer -> Fay ())
               -> [DOMElement]
dryerExtraRows editing dryer setDryer = [
  row'
    editing
    "Poznámka k sušičce"
    (SetValue $ MD.note dryer)
    (eventString >=> (\s -> setDryer $ dryer { MD.note = s })) ]
