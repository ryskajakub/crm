{-# LANGUAGE PackageImports #-}

module HaskellReact.Tag.Construct where

import HaskellReact.Event
import FFI (Defined(Undefined), ffi, Automatic)
import "fay-base" Data.Text (Text)
import Prelude hiding (id)

data DOMElement

class Renderable a

instance (Renderable a) => Renderable [a]
instance Renderable Text
instance Renderable DOMElement

data NoAttributes = NoAttributes {}

data Attributes = Attributes {
  className :: Defined String
  , onClick :: Defined ( SyntheticMouseEvent -> Fay() )
  , id :: Defined String
}

defaultAttributes :: Attributes
defaultAttributes = Attributes {
  className = Undefined
  , onClick = Undefined
  , id = Undefined
}

-- | Unsafely create a html tag
constructDOMElement :: (Renderable a)
                    => String -- name of tag
                    -> Attributes -- html attributes common for all elements
                    -> Automatic b -- tag specific attributes
                    -> Automatic a -- child
                    -> DOMElement

constructDOMElement = ffi " require('../files/ReactWrapper').constructDOMElement(%1, %2, %4, %3) "
