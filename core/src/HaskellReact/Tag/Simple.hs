{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellReact.Tag.Simple (
  span, span'
  , div, div'
  , li, li'
  , ul, ul'
) where

import HaskellReact.Tag.Construct
import FFI (Automatic)
import Prelude hiding (div, span)
import "fay-base" Data.Text (Text, fromString)

constructSimple :: Renderable a 
               => Text 
               -> Attributes 
               -> Automatic a 
               -> DOMElement
constructSimple name attributes children = constructDOMElement name attributes (NoAttributes {}) children

span' :: Renderable a => Attributes -> Automatic a -> DOMElement
span' = constructSimple "span"

span :: Renderable a => Automatic a -> DOMElement
span = span' defaultAttributes

div' :: Renderable a => Attributes -> Automatic a -> DOMElement
div' = constructSimple "div"

div :: Renderable a => Automatic a -> DOMElement
div = div' defaultAttributes

li' :: (Renderable a) => Attributes -> Automatic a -> DOMElement
li' = constructSimple "li"

li :: Renderable a => Automatic a -> DOMElement
li = li' defaultAttributes

ul' :: (Renderable a) => Attributes -> Automatic a -> DOMElement
ul' = constructSimple "ul"

ul :: Renderable a => Automatic a -> DOMElement
ul = ul' defaultAttributes
