{-# LANGUAGE PackageImports #-}

module HaskellReact.Tag.Anchor where

import FFI (Defined(Undefined))
import HaskellReact.Tag.Construct
import "fay-base" Data.Text (Text)

type URL = Text
type Rel = Text
type Target = Text

data AnchorAttributes = AnchorAttributes {
  href :: Defined URL
  , rel :: Defined Rel
  , target :: Defined Target
}

defaultAnchorAttributes :: AnchorAttributes
defaultAnchorAttributes = AnchorAttributes {
  href = Undefined
  , rel = Undefined
  , target = Undefined
}

a' :: (Renderable a) => Attributes -> AnchorAttributes -> a -> DOMElement
a' = constructDOMElement "a" 

a :: (Renderable a) => AnchorAttributes -> a -> DOMElement
a = a' defaultAttributes
