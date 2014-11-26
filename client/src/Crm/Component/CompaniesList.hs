{-# LANGUAGE PackageImports #-}

module Crm.Component.CompaniesList (
  companiesList
) where

import HaskellReact
import Crm.Component.Navigation (navigation)
import "fay-base" Data.Text (pack)
import Prelude hiding (div)

companiesList :: Int -> ReadFay DOMElement
companiesList num = readFayReturn $ let 
  element = div $ pack ("Companies list" ++ show num)
  elementWithNavigation = reactInstance2DOM $ classInstance $ navigation element
  in elementWithNavigation
