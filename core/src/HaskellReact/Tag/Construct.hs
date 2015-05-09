{-# LANGUAGE PackageImports #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HaskellReact.Tag.Construct where

import "fay-base" FFI (Defined(Defined, Undefined), ffi, Automatic, Nullable)
import "fay-base" Data.Text 
import "fay-base" Unsafe.Coerce (unsafeCoerce)
import "fay-base" Prelude hiding (id, intercalate)

import HaskellReact.Event

-- | Opaque data type that represents a element in the react virtual dom
data DOMElement

-- | Elements that can be part of the react virtual dom are instances of this class
class Renderable a

instance Renderable a => Renderable [a]
instance Renderable a => Renderable (Nullable a)
instance Renderable Text
instance Renderable DOMElement

-- | convert the text to the dom element, used mainly in lists, so each element will have the same type
text2DOM :: Text -> DOMElement
text2DOM = unsafeCoerce

data NoAttributes = NoAttributes {}

-- | attributes, that can be given to any tag
data Attributes = Attributes {
  className :: Defined Text , -- ^ html class names, must be separated by spaces
  onClick :: Defined ( SyntheticMouseEvent -> Fay() ) , -- ^ click handler
  id :: Defined Text ,
  key :: Defined Text } -- ^ html unique id of the element

defaultAttributes :: Attributes
defaultAttributes = Attributes {
  className = Undefined ,
  onClick = Undefined ,
  id = Undefined ,
  key = Undefined }

click :: Fay () -> Attributes
click callback = mkAttrs {
  onClick = Defined $ const callback }

mkAttrs :: Attributes
mkAttrs = defaultAttributes

-- | create row attributes containing only row key
row :: Text -> Attributes
row text = mkAttrs {
  key = Defined text }

-- | create attributes containing only class with single value
class' :: Text -> Attributes
class' className' = mkAttrs {
  className = Defined $ className' }

-- | create attributes containing only class with multiple values
class'' :: [Text] -> Attributes
class'' classNames = class' $ intercalate (pack " ") classNames

-- | unsafely create a html tag
constructDOMElement :: (Renderable a)
                    => Text -- ^ name of tag
                    -> Attributes -- ^ html attributes common for all elements
                    -> Automatic b -- ^ tag specific attributes
                    -> Automatic a -- ^ children
                    -> DOMElement
constructDOMElement = ffi "\
\ (function(elementName, attributes, children, moreAttributes) {\
  \ var React = require('react');\
  \ var obj = {};\
  \ var addAttributes = function (attrs) {\
    \ for (key in attrs) {\
      \ if (key !== 'instance') {\
        \ var newKey = (key.charAt(key.length - 1) == '_' ? key.substring(0, key.length - 1) : key);\
        \ var newerKey = newKey.replace(/_/g, '-');\
        \ obj[newerKey] = attrs[key];\
      \ }\
    \ }\
  \ };\
  \ addAttributes(attributes);\
  \ addAttributes(moreAttributes);\
  \ childrenNullable = children['instance'] != null && children['instance'] == 'Null' ? null : children;\
  \ return React.DOM[elementName](obj, childrenNullable);\
\ })(%1, %2, %4, %3)\
\ "
