{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Crm.Component.CompanyDetail (
  companyDetail
) where

import HaskellReact
import Crm.Component.Navigation (navigation)
import Crm.Shared.Data
import "fay-base" Data.Text (fromString, Text, unpack, pack)
import Prelude hiding (div, span)
import Data.Var (Var, subscribeAndRead)
import HaskellReact.BackboneRouter (BackboneRouter)
import Crm.Component.Data

import "fay-base" Debug.Trace

companyDetail :: MyData
              -> Company
              -> DOMElement
companyDetail myData company = div $ pack $ show company
