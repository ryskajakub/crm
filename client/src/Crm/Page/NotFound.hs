{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Page.NotFound (
  notFound ) where

import "fay-base" Data.Text (fromString)
import "fay-base" Prelude
import "fay-base" FFI (Defined(Defined))

import HaskellReact
import qualified HaskellReact.Bootstrap as B

notFound :: DOMElement
notFound = let
  text = "Stránka nenalezena"
  in B.grid $ B.row $ B.col ((B.mkColProps 6) { B.mdOffset = Defined 3 }) text
