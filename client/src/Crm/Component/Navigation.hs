{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

module Crm.Component.Navigation where

import HaskellReact
import "fay-base" Data.Text (pack, Text, fromString)
import Prelude hiding (span, div, elem)
import HaskellReact.Bootstrap (navBar, nav)
import HaskellReact.BackboneRouter (link, BackboneRouter)
import qualified HaskellReact.Bootstrap.Glyphicon as G

navigation :: Maybe BackboneRouter -> DOMElement -> ReactClass a
navigation router innerElement = declareReactClass $ reactData ("Navigation") (Empty {}) (
  \reactThis -> readFayReturn $
    div [
      reactInstance2DOM $ navBar $ nav [
        li $ link [G.list, text2DOM " Seznam firem"] "" router
      ]
      , innerElement
    ]
  )
