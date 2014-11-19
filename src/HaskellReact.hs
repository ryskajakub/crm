{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports #-}

module HaskellReact (
  module HaskellReact.Tag.Construct
  , module HaskellReact.Tag.Simple
  , module HaskellReact.Event
  , module HaskellReact.Component
  , AAttributes(..)
  , aAttributesDefaults
  , a
  , textElement
  , phantom
) where 

import FFI
import "fay-base" Data.Text (Text, pack)
import Prelude hiding (id, span, div)
import HaskellReact.Event
import HaskellReact.Tag.Construct
import HaskellReact.Tag.Simple
import HaskellReact.Component

type URL = Text
type Rel = Text
type Target = Text

data AAttributes = AAttributes {
  href :: Defined URL
  , rel :: Defined Rel
  , target :: Defined Target
}

aAttributesDefaults :: AAttributes
aAttributesDefaults = AAttributes Undefined Undefined Undefined

a :: (Renderable x) => Attributes -> AAttributes -> x -> DOMElement
a = ffi " require('../files/ReactWrapper').constructDOMElement(\"a\", %1, Fay$$_(%3), %2) "

phantom :: a -> b
phantom = ffi " %1 "

textElement :: String -> DOMElement
textElement = phantom . pack
