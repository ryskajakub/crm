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
  section , section' ,
  main , main' ,
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
  strong , strong'
) where

import "fay-base" Prelude hiding (div, span)
import "fay-base" Data.Text (Text, fromString)

import HaskellReact.Tag.Construct

constructSimple :: Renderable a
                => Text
                -> Attributes
                -> a
                -> DOMElement
constructSimple name attributes children = constructDOMElement name attributes (NoAttributes {}) children

strong' :: Renderable a => Attributes -> a -> DOMElement
strong' = constructSimple "strong"

strong :: Renderable a => a -> DOMElement
strong = strong' defaultAttributes

span' :: Renderable a => Attributes -> a -> DOMElement
span' = constructSimple "span"

span :: Renderable a => a -> DOMElement
span = span' defaultAttributes

div' :: Renderable a => Attributes -> a -> DOMElement
div' = constructSimple "div"

div :: Renderable a => a -> DOMElement
div = div' defaultAttributes

p' :: Renderable a => Attributes -> a -> DOMElement
p' = constructSimple "p"

p :: Renderable a => a -> DOMElement
p = p' defaultAttributes

li' :: (Renderable a) => Attributes -> a -> DOMElement
li' = constructSimple "li"

li :: Renderable a => a -> DOMElement
li = li' defaultAttributes

ol :: Renderable a => a -> DOMElement
ol = ol' defaultAttributes

ol' :: Renderable a => Attributes -> a -> DOMElement
ol' = constructSimple "ol"

ul' :: (Renderable a) => Attributes -> a -> DOMElement
ul' = constructSimple "ul"

ul :: Renderable a => a -> DOMElement
ul = ul' defaultAttributes

main' :: (Renderable a) => Attributes -> a -> DOMElement
main' = constructSimple "main"

main :: Renderable a => a -> DOMElement
main = main' defaultAttributes

section :: (Renderable a) => a -> DOMElement
section = section' defaultAttributes

section' :: (Renderable a) => Attributes -> a -> DOMElement
section' = constructSimple "section"

th :: (Renderable a) => a -> DOMElement
th = th' defaultAttributes

th' :: (Renderable a) => Attributes -> a -> DOMElement
th' = constructSimple "th"

tr :: (Renderable a) => a -> DOMElement
tr = tr' defaultAttributes

tr' :: (Renderable a) => Attributes -> a -> DOMElement
tr' = constructSimple "tr"

td' :: (Renderable a) => Attributes -> a -> DOMElement
td' = constructSimple "td"

td :: (Renderable a) => a -> DOMElement
td = td' defaultAttributes

thead :: (Renderable a) => a -> DOMElement
thead = thead' defaultAttributes

thead' :: (Renderable a) => Attributes -> a -> DOMElement
thead' = constructSimple "thead"

tbody :: (Renderable a) => a -> DOMElement
tbody = tbody' defaultAttributes

tbody' :: (Renderable a) => Attributes -> a -> DOMElement
tbody' = constructSimple "tbody"

dl :: (Renderable a) => a -> DOMElement
dl = constructSimple "dl" defaultAttributes

dt :: (Renderable a) => a -> DOMElement
dt = constructSimple "dt" defaultAttributes

dd :: (Renderable a) => a -> DOMElement
dd = constructSimple "dd" defaultAttributes

h1 :: (Renderable a) => a -> DOMElement
h1 = constructSimple "h1" defaultAttributes

h2 :: (Renderable a) => a -> DOMElement
h2 = constructSimple "h2" defaultAttributes

h3 :: (Renderable a) => a -> DOMElement
h3 = constructSimple "h3" defaultAttributes

h4 :: (Renderable a) => a -> DOMElement
h4 = constructSimple "h4" defaultAttributes

h5 :: (Renderable a) => a -> DOMElement
h5 = constructSimple "h5" defaultAttributes

h6 :: (Renderable a) => a -> DOMElement
h6 = constructSimple "h6" defaultAttributes

form :: (Renderable a) => a -> DOMElement
form = constructSimple "form" defaultAttributes

form' :: (Renderable a) => Attributes -> a -> DOMElement
form' = constructSimple "form"

label' :: (Renderable a) => Attributes -> a -> DOMElement
label' = constructSimple "label"

label :: (Renderable a) => a -> DOMElement
label = label' defaultAttributes

nav' :: (Renderable a) => Attributes -> a -> DOMElement
nav' = constructSimple "nav"
