{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HaskellReact.Bootstrap.Nav (
  nav
) where

import "fay-base" Prelude
import "fay-base" Data.Text (Text, fromString)
import "fay-base" FFI (Defined(Undefined, Defined))

import HaskellReact
import HaskellReact.Tag.Hyperlink (a)

nav :: [DOMElement] -- ^ list of links
    -> DOMElement
nav links = nav' (class'' ["navbar","navbar-default"]) $ ul' (class'' ["nav","navbar-nav"]) lis
  where lis = map (\link -> li link) links
