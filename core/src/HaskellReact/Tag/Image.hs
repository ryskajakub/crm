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
import "fay-base" FFI (Defined (Undefined))

import HaskellReact

data ImageAttributes = ImageAttributes {
  src :: Text ,
  width :: Defined Int ,
  height :: Defined Int }

mkImageAttrs :: Text -> ImageAttributes
mkImageAttrs src' = ImageAttributes src' Undefined Undefined

image' :: Attributes -> ImageAttributes -> DOMElement
image' attrs imgAttrs = constructDOMElement "img" attrs imgAttrs ([]::[DOMElement])
