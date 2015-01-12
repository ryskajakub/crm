{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HaskellReact.Bootstrap.Glyphicon where

import "fay-base" Data.Text (Text, fromString)
import "fay-base" Prelude

import HaskellReact
import HaskellReact.Bootstrap

data GlyphiconProps = GlyphiconProps {
  glyph :: Text
}

glyphicon' :: GlyphiconProps -> DOMElement
glyphicon' props = reactBootstrap "Glyphicon" props ([]::[DOMElement])

glyphicon :: Text -> DOMElement
glyphicon glyphIdentificator = glyphicon' (GlyphiconProps glyphIdentificator)

plus :: DOMElement
plus = glyphicon "plus"

list :: DOMElement
list = glyphicon "list"

pencil :: DOMElement
pencil = glyphicon "pencil"
