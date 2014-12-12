{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HaskellReact.Bootstrap.Glyphicon where

import "fay-base" Data.Text (Text, fromString)
import HaskellReact
import HaskellReact.Bootstrap
import "fay-base" Prelude

data GlyphiconProps = GlyphiconProps {
  glyph :: Text
}

glyphicon' :: GlyphiconProps -> DOMElement
glyphicon' props = reactInstance2DOM $ reactBootstrap "Glyphicon" props ([]::[DOMElement])

glyphicon :: Text -> DOMElement
glyphicon glyph = glyphicon' (GlyphiconProps glyph)

plus :: DOMElement
plus = glyphicon "plus"

list :: DOMElement
list = glyphicon "list"
