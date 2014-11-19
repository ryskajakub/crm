{-# LANGUAGE PackageImports #-}

module HaskellReact.Tag.Hyperlink where

import FFI (Defined(Undefined))
import HaskellReact.Tag.Construct
import "fay-base" Data.Text (Text)

type URL = Text
type Rel = Text
type Target = Text

data HyperlinkAttributes = HyperlinkAttributes {
  href :: Defined URL
  , rel :: Defined Rel
  , target :: Defined Target
}

defaultHyperlinkAttributes :: HyperlinkAttributes
defaultHyperlinkAttributes = HyperlinkAttributes {
  href = Undefined
  , rel = Undefined
  , target = Undefined
}

a' :: (Renderable a) => Attributes -> HyperlinkAttributes -> a -> DOMElement
a' = constructDOMElement "a" 

a :: (Renderable a) => HyperlinkAttributes -> a -> DOMElement
a = a' defaultAttributes
