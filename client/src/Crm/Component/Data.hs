{-# LANGUAGE PackageImports #-}

module Crm.Component.Data where

import HaskellReact.BackboneRouter
import "fay-base" Data.Text (Text)

data MyData = MyData {
  router :: BackboneRouter
}
