module Crm.Component.Data where

import HaskellReact.BackboneRouter

data RouterState = CompaniesList

data MainState = MainState {
  routerState :: RouterState
  , router :: Maybe BackboneRouter
}
