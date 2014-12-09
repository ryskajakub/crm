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

import "fay-base" Debug.Trace

companyDetail :: Maybe BackboneRouter -- ^ Router from which the link can be created
              -> Var [Company]
              -> Int -- ^ Company id
              -> ReadFay DOMElement
companyDetail router' companyVar cId = readFayReturn $ let
  data' = reactData "CompanyDetail" (Empty {}) (\reactThis ->
      readFayReturn $ reactInstance2DOM $ classInstance $ navigation router' (div "ahoj")
    )
  in reactInstance2DOM $ classInstance $ declareReactClass data'
