{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellReact.Bootstrap.Glyphicon where

import "fay-base" Data.Text (Text, fromString)
import HaskellReact
import HaskellReact.Bootstrap
import Prelude

data GlyphiconProps = GlyphiconProps {
  glyph :: Text
}

glyphicon' :: GlyphiconProps -> DOMElement
glyphicon' props = reactInstance2DOM $ reactBootstrap "Glyphicon" props ([]::[DOMElement])

glyphicon :: Text -> DOMElement
glyphicon glyph = glyphicon' (GlyphiconProps glyph)

plus :: DOMElement
plus = glyphicon "plus"
