{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HaskellReact.Tag.Simple (
  span , span' ,
  div , div' ,
  p , p' ,
  li , li' ,
  ul , ul' ,
  ol , ol' ,
  section ,
  main ,
  tr , tr' ,
  thead , thead' ,
  tbody , tbody' ,
  th , th' ,
  td , td' ,
  dt , dd , dl ,
  h1 , h2 , h3 ,
  h4 , h5 , h6 ,
  form , form' ,
  label' , label ,
  nav' ,
  strong
) where

import "fay-base" FFI (Automatic)
import "fay-base" Prelude hiding (div, span)
import "fay-base" Data.Text (Text, fromString)

import HaskellReact.Tag.Construct

constructSimple :: Renderable a
                => Text
                -> Attributes
                -> Automatic a
                -> DOMElement
constructSimple name attributes children = constructDOMElement name attributes (NoAttributes {}) children

strong :: Renderable a => a -> DOMElement
strong = constructSimple "strong" defaultAttributes

span' :: Renderable a => Attributes -> Automatic a -> DOMElement
span' = constructSimple "span"

span :: Renderable a => Automatic a -> DOMElement
span = span' defaultAttributes

div' :: Renderable a => Attributes -> Automatic a -> DOMElement
div' = constructSimple "div"

div :: Renderable a => Automatic a -> DOMElement
div = div' defaultAttributes

p' :: Renderable a => Attributes -> a -> DOMElement
p' = constructSimple "p"

p :: Renderable a => a -> DOMElement
p = p' defaultAttributes

li' :: (Renderable a) => Attributes -> Automatic a -> DOMElement
li' = constructSimple "li"

li :: Renderable a => Automatic a -> DOMElement
li = li' defaultAttributes

ol :: Renderable a => Automatic a -> DOMElement
ol = ol' defaultAttributes

ol' :: Renderable a => Attributes -> Automatic a -> DOMElement
ol' = constructSimple "ol"

ul' :: (Renderable a) => Attributes -> Automatic a -> DOMElement
ul' = constructSimple "ul"

ul :: Renderable a => Automatic a -> DOMElement
ul = ul' defaultAttributes

main :: (Renderable a) => Automatic a -> DOMElement
main = constructSimple "main" defaultAttributes

section :: (Renderable a) => Automatic a -> DOMElement
section = constructSimple "section" defaultAttributes

th :: (Renderable a) => Automatic a -> DOMElement
th = th' defaultAttributes

th' :: (Renderable a) => Attributes -> Automatic a -> DOMElement
th' = constructSimple "th"

tr :: (Renderable a) => Automatic a -> DOMElement
tr = tr' defaultAttributes

tr' :: (Renderable a) => Attributes -> Automatic a -> DOMElement
tr' = constructSimple "tr"

td' :: (Renderable a) => Attributes -> Automatic a -> DOMElement
td' = constructSimple "td"

td :: (Renderable a) => Automatic a -> DOMElement
td = td' defaultAttributes

thead :: (Renderable a) => Automatic a -> DOMElement
thead = thead' defaultAttributes

thead' :: (Renderable a) => Attributes -> Automatic a -> DOMElement
thead' = constructSimple "thead"

tbody :: (Renderable a) => Automatic a -> DOMElement
tbody = tbody' defaultAttributes

tbody' :: (Renderable a) => Attributes -> Automatic a -> DOMElement
tbody' = constructSimple "tbody"

dl :: (Renderable a) => Automatic a -> DOMElement
dl = constructSimple "dl" defaultAttributes

dt :: (Renderable a) => Automatic a -> DOMElement
dt = constructSimple "dt" defaultAttributes

dd :: (Renderable a) => Automatic a -> DOMElement
dd = constructSimple "dd" defaultAttributes

h1 :: (Renderable a) => Automatic a -> DOMElement
h1 = constructSimple "h1" defaultAttributes

h2 :: (Renderable a) => Automatic a -> DOMElement
h2 = constructSimple "h2" defaultAttributes

h3 :: (Renderable a) => a -> DOMElement
h3 = constructSimple "h3" defaultAttributes

h4 :: (Renderable a) => a -> DOMElement
h4 = constructSimple "h4" defaultAttributes

h5 :: (Renderable a) => a -> DOMElement
h5 = constructSimple "h5" defaultAttributes

h6 :: (Renderable a) => a -> DOMElement
h6 = constructSimple "h6" defaultAttributes

form :: (Renderable a) => Automatic a -> DOMElement
form = constructSimple "form" defaultAttributes

form' :: (Renderable a) => Attributes -> Automatic a -> DOMElement
form' = constructSimple "form"

label' :: (Renderable a) => Attributes -> Automatic a -> DOMElement
label' = constructSimple "label"

label :: (Renderable a) => a -> DOMElement
label = label' defaultAttributes

nav' :: (Renderable a) => Attributes -> a -> DOMElement
nav' = constructSimple "nav"
