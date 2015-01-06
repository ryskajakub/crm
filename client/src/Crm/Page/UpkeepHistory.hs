{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Page.UpkeepHistory (
  upkeepHistory ) where

import "fay-base" Data.Text (fromString)
import "fay-base" Prelude hiding (div, span, id)

import HaskellReact as HR
import qualified HaskellReact.Bootstrap as B

import qualified Crm.Shared.Upkeep as U
import Crm.Helpers (displayDate)

upkeepHistory :: [U.Upkeep'']
              -> DOMElement
upkeepHistory upkeeps = let
  upkeepHtml (_, upkeep) =
    B.row $ B.col (B.mkColProps 12) (displayDate $ U.upkeepDate upkeep)
  in div [
    h2 "Historie servisů" ,
    B.grid $ map upkeepHtml upkeeps ]
