{-# LANGUAGE PackageImports #-}

module Crm.Component.CompaniesList (
  companiesList
) where

import HaskellReact
import Crm.Component.Navigation (navigation)
import "fay-base" Data.Text (pack)
import Prelude hiding (div)
import HaskellReact.BackboneRouter (BackboneRouter)

companiesList :: Maybe BackboneRouter -- ^ Router from which the link can be created
              -> ReadFay DOMElement
companiesList router = readFayReturn $ let 
  element = div $ pack ("Companies list")
  elementWithNavigation = reactInstance2DOM $ classInstance $ navigation router element
  in elementWithNavigation
