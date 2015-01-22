{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Page.UpkeepHistory (
  upkeepHistory ) where

import "fay-base" Data.Text (fromString, pack)
import "fay-base" Prelude hiding (div, span, id)

import HaskellReact
import qualified HaskellReact.Bootstrap as B

import qualified Crm.Shared.Upkeep as U
import qualified Crm.Shared.Employee as E
import Crm.Helpers (displayDate)

import Debug.Trace

upkeepHistory :: [(U.Upkeep'', Maybe E.Employee')]
              -> DOMElement
upkeepHistory upkeeps = let
  upkeepHtml ((_, upkeep), maybeEmployee) = let
    employeeText = maybe ("---") (pack . E.name . snd) maybeEmployee
    in trace (show employeeText) [
      B.row $ B.col (B.mkColProps 12) (h3 $ displayDate $ U.upkeepDate upkeep) ,
      B.row $ B.col (B.mkColProps 12) (p [ strong "Servisman", text2DOM " " , 
      text2DOM employeeText ])]
  in div [
    h2 "Historie servisů" ,
    B.grid $ map upkeepHtml upkeeps ]
