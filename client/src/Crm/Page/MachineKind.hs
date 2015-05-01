{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Page.MachineKind (
  compressorExtraRows ,
  dryerExtraRows) where

import "fay-base" Prelude hiding (div, span, id)

import HaskellReact

compressorExtraRows :: [DOMElement]
compressorExtraRows = []

dryerExtraRows :: [DOMElement]
dryerExtraRows = []
