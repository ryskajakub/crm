{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Crm.Page.NotFound (
  notFound ) where

import           Data.Text              (fromString)
import           Prelude
import           FFI                    (Defined(Defined))

import           HaskellReact
import qualified HaskellReact.Bootstrap as B

notFound :: DOMElement
notFound = let
  text = "Stránka nenalezena"
  in B.grid $ B.row $ B.col ((B.mkColProps 6) { B.mdOffset = Defined 3 }) text
