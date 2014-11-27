{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Crm.Component.CompaniesList (
  companiesList
) where

import HaskellReact
import Crm.Component.Navigation (navigation)
import "fay-base" Data.Text (fromString)
import Prelude hiding (div)
import HaskellReact.BackboneRouter (BackboneRouter)
import qualified HaskellReact.Bootstrap as B
import qualified HaskellReact.Bootstrap.Glyphicon as G

companiesList :: Maybe BackboneRouter -- ^ Router from which the link can be created
              -> ReadFay DOMElement
companiesList router = readFayReturn $ let 
  element = main $
    section [
      B.button [
        G.plus
        , text2DOM "Přidat firmu"
      ]
    ]
  elementWithNavigation = reactInstance2DOM $ classInstance $ navigation router element
  in elementWithNavigation
