{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Component.Data where

import HaskellReact.BackboneRouter
import "fay-base" Data.Text (Text)
import "fay-base" Prelude
import qualified Crm.Shared.Machine as M
import qualified Crm.Shared.Company as C

data MyData = MyData {
  router :: BackboneRouter
}

data NavigationState =
  FrontPage
  | CompanyDetail {
    companyId :: Int
    , company :: C.Company
    , editing :: Bool
    , companyMachines :: [M.Machine] }

data AppState = AppState {
  navigation :: NavigationState
  , companies :: [(Int, C.Company)]
  , machines :: [(Int, M.Machine)]
}

defaultAppState :: AppState
defaultAppState = AppState {
  navigation = FrontPage
  , companies = []
  , machines = []
}
