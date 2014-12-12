{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Component.Navigation where

import HaskellReact
import "fay-base" Data.Text (pack, Text, fromString)
import "fay-base" Prelude hiding (span, div, elem)
import HaskellReact.Bootstrap (navBar, nav)
import HaskellReact.BackboneRouter (link, BackboneRouter)
import qualified HaskellReact.Bootstrap.Glyphicon as G
import Crm.Component.Data (MyData, router)

navigation :: MyData -> DOMElement -> Fay ()
navigation myData body = 
  simpleReactBody $ div [
    reactInstance2DOM $ navBar $ nav [
      li $ link [G.list, text2DOM " Seznam firem"] "" (router myData)
    ]
    , body
  ]
