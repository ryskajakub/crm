{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

module Crm.Component.Navigation where

import HaskellReact
import "fay-base" Data.Text (pack, Text, fromString)
import Prelude hiding (span, div, elem)
import HaskellReact.Bootstrap (navBar, nav)
import HaskellReact.BackboneRouter (link, BackboneRouter)

navigation :: Maybe BackboneRouter -> DOMElement -> ReactClass a
navigation router innerElement = declareReactClass $ reactData ("Navigation") (Empty {}) (
  \reactThis -> readFayReturn $
    div [
      reactInstance2DOM $ navBar $ nav [
        li $ link "Seznam firem" "" (router)
        , li $ link "Naplánované servisy" "/company/555" (router)
      ]
      , innerElement
    ]
  )
{-

data NavLinkData = NavLinkData {
  to :: Text
}

navLink :: ReactClass NavLinkData
navLink = declareStateless ("NavLink") $ \reactThis -> let RF return (>>=) (>>) = rf in do
  p <- props reactThis
  readFayReturn $ li $ link (LinkProps $ to p) (to p)
-}
