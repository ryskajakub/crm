{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Crm.Component.CompaniesList (
  companiesList
) where

import HaskellReact
import Crm.Component.Navigation (navigation)
import Crm.Component.Data
import "fay-base" Data.Text (fromString, Text)
import Prelude hiding (div)
import HaskellReact.BackboneRouter (BackboneRouter)
import qualified HaskellReact.Bootstrap as B
import qualified HaskellReact.Bootstrap.Glyphicon as G

companies :: [Company]
companies = [
  Company "F 1" "50 dní"
  , Company "F 2" "2 roky"
  , Company "F 5" "8 roků"
  ]

companiesList :: Maybe BackboneRouter -- ^ Router from which the link can be created
              -> ReadFay DOMElement
companiesList router = readFayReturn $ let 
  mkRow company = tr [
    td $ name company
    , td $ plant company
    ]
  rows = map mkRow companies 
  element = main [
    section $
      B.button [
        G.plus
        , text2DOM "Přidat firmu"
      ]
    , section $ 
      B.table [
        thead $ tr [
          th "Název firmy"
          , th "Platnost servisu vyprší za"
        ]
        , tbody rows
      ]
    ]
  elementWithNavigation = reactInstance2DOM $ classInstance $ navigation router element
  in elementWithNavigation
