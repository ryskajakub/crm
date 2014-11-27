{-# LANGUAGE PackageImports #-}

module Crm.Component.Data where

import HaskellReact.BackboneRouter
import "fay-base" Data.Text (Text)

data RouterState = CompaniesList

data MainState = MainState {
  routerState :: RouterState
  , router :: Maybe BackboneRouter
}
