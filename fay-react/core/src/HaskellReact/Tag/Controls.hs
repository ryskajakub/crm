{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HaskellReact.Tag.Controls (
  button, button' ) where

import "fay-base" Prelude hiding (div, span)
import "fay-base" Data.Text (Text, fromString)

import HaskellReact.Tag.Simple
import HaskellReact.Tag.Construct

button' :: Renderable a => Attributes -> a -> DOMElement
button' = constructSimple "button"

button :: Renderable a => a -> DOMElement
button = button' defaultAttributes
