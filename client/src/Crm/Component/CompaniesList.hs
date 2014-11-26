{-# LANGUAGE PackageImports #-}

module Crm.Component.CompaniesList (
  companiesList
) where

import HaskellReact
import Crm.Component.Navigation (navigation)
import "fay-base" Data.Text (pack)
import Prelude hiding (div)
import HaskellReact.BackboneRouter (BackboneRouter)

companiesList :: Maybe BackboneRouter -> Int -> ReadFay DOMElement
companiesList router num = readFayReturn $ let 
  element = div $ pack ("Companies list" ++ show num)
  elementWithNavigation = reactInstance2DOM $ classInstance $ navigation router element
  in elementWithNavigation
