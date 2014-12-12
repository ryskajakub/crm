{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Component.Data where

import HaskellReact.BackboneRouter
import "fay-base" Data.Text (Text)
import "fay-base" Prelude

data MyData = MyData {
  router :: BackboneRouter
}
