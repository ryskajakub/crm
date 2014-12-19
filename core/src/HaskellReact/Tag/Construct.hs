{-# LANGUAGE PackageImports #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HaskellReact.Tag.Construct where

import HaskellReact.Event
import FFI (Defined(Defined, Undefined), ffi, Automatic)
import "fay-base" Data.Text 
import "fay-base" Unsafe.Coerce (unsafeCoerce)
import "fay-base" Prelude hiding (id, intercalate)

import "fay-base" Debug.Trace

data DOMElement

class Renderable a

instance Renderable a => Renderable [a]
instance Renderable Text
instance Renderable DOMElement

text2DOM :: Text -> DOMElement
text2DOM = unsafeCoerce

data NoAttributes = NoAttributes {}

data Attributes = Attributes {
  className :: Defined Text
  , onClick :: Defined ( SyntheticMouseEvent -> Fay() )
  , id :: Defined Text
}

defaultAttributes :: Attributes
defaultAttributes = Attributes {
  className = Undefined
  , onClick = Undefined
  , id = Undefined
}

mkAttrs :: Attributes
mkAttrs = defaultAttributes

class' :: Text -> Attributes
class' className' = mkAttrs {
  className = Defined $ className'
  }

class'' :: [Text] -> Attributes
class'' classNames = class' $ intercalate (pack " ") classNames

-- | Unsafely create a html tag
constructDOMElement :: (Renderable a)
                    => Text -- name of tag
                    -> Attributes -- html attributes common for all elements
                    -> Automatic b -- tag specific attributes
                    -> Automatic a -- children
                    -> DOMElement

constructDOMElement = ffi "\
\ (function(elementName, attributes, children, moreAttributes) {\
  \ var React = require('react');\
  \ var obj = {};\
  \ var escapeKey = function(key) {\
    \ return (key.charAt(key.length - 1) === '_' ? key.substring(0, key.length - 1) : key);\
  \ };\
  \ var addAttributes = function (attrs) {\
    \ for (key in attrs) {\
      \ if (key !== 'instance') {\
        \ obj[key] = attrs[key];\
      \ }\
    \ }\
  \ };\
  \ addAttributes(attributes);\
  \ addAttributes(moreAttributes);\
  \ return React.DOM[elementName](obj, children);\
\ })(%1, %2, %4, %3)\
\ "
