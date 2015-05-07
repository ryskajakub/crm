{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Page.Dashboard (
  dashboard ) where

import "fay-base" Data.Text (fromString) 
import "fay-base" Prelude hiding (div, span, id)
import FFI

import HaskellReact
import qualified HaskellReact.Bootstrap as B

dashboard :: DOMElement
dashboard = B.grid $ B.row $ B.col (B.mkColProps 12) $ 
  div' (mkAttrs { id = Defined "half-container" }) $ 
    div' (mkAttrs { id = Defined "dashboard-map" }) ""
