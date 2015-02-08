{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HaskellReact.Tag.Image (
  image' ,
  mkImageAttrs ,
  ImageAttributes (..) ) where

import "fay-base" Prelude
import "fay-base" Data.Text (Text, fromString)

import HaskellReact

data ImageAttributes = ImageAttributes {
  src :: Text }

mkImageAttrs :: Text -> ImageAttributes
mkImageAttrs src' = ImageAttributes src'

image' :: Attributes -> ImageAttributes -> DOMElement
image' attrs imgAttrs = constructDOMElement "img" attrs imgAttrs ([]::[DOMElement])
