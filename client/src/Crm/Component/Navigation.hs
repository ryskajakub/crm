{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE PackageImports #-}

module Crm.Component.Navigation (
  navigation
) where

import HaskellReact
import "fay-base" Data.Text (pack, Text)
import Prelude hiding (span, div, elem)
import HaskellReact.Router (link, routeHandler, LinkProps(LinkProps))
import HaskellReact.Bootstrap (navBar, nav)

data' :: ReactData Empty a
data' = defaultReactData (pack "Navigation") (Empty {}) (
  const $ readFayReturn $
    div [
      navBar $ nav [
        classInstance' navLink (NavLinkData $ pack "default")
        , classInstance' navLink (NavLinkData $ pack "users")
      ]
      , phantom $ routeHandler
    ]
  )

navigation :: ReactClass a
navigation = declareReactClass data'

data NavLinkData = NavLinkData {
  to :: Text
}

navLink :: ReactClass NavLinkData
navLink = declareStateless (pack "NavLink") $ \reactThis -> let RF return (>>=) (>>) = rf in do
  p <- props reactThis
  readFayReturn $ li $ link (LinkProps $ to p) (to p)
