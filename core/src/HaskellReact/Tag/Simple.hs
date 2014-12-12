{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HaskellReact.Tag.Simple (
  span, span'
  , div, div'
  , li, li'
  , ul, ul'
  , section
  , main
  , tr 
  , thead
  , tbody
  , th
  , td
) where

import HaskellReact.Tag.Construct
import FFI (Automatic)
import "fay-base" Prelude hiding (div, span)
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

main :: (Renderable a) => Automatic a -> DOMElement
main = constructSimple "main" defaultAttributes

section :: (Renderable a) => Automatic a -> DOMElement
section = constructSimple "section" defaultAttributes

th :: (Renderable a) => Automatic a -> DOMElement
th = constructSimple "th" defaultAttributes

tr :: (Renderable a) => Automatic a -> DOMElement
tr = constructSimple "tr" defaultAttributes

td :: (Renderable a) => Automatic a -> DOMElement
td = constructSimple "td" defaultAttributes

thead :: (Renderable a) => Automatic a -> DOMElement
thead = constructSimple "thead" defaultAttributes

tbody :: (Renderable a) => Automatic a -> DOMElement
tbody = constructSimple "tbody" defaultAttributes
