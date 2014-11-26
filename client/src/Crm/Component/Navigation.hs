{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

module Crm.Component.Navigation (
  navigation
) where

import HaskellReact
import "fay-base" Data.Text (pack, Text, fromString)
import Prelude hiding (span, div, elem)
import HaskellReact.Bootstrap (navBar, nav)

navigation :: DOMElement -> ReactClass a
navigation innerElement = declareReactClass $ reactData ("Navigation") (Empty {}) (
  \reactThis ->
    readFayReturn $
      div [
        span "navigace"
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
