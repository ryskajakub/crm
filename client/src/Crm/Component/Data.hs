module Crm.Component.Data where

import HaskellReact.BackboneRouter

data RouterState = Slash | Company Int

data MainState = MainState {
  routerState :: RouterState
  , router :: Maybe BackboneRouter
}
