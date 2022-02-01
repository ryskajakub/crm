{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HaskellReact.Tag.Hyperlink (
  a , a', a'', a'''
  , HyperlinkAttributes(HyperlinkAttributes)
  , href
  , rel
  , target
  , defaultHyperlinkAttributes, mkAAttrs
  , Rel
  , alternate
  , author
  , bookmark
  , help
  , license
  , next
  , nofollow
  , noreferrer
  , prefetch
  , prev
  , search
  , tag
  , HyperlinkTarget
  , blank
  , parent
  , self
  , top
  , framename
) where

import "fay-base" FFI (Defined(Undefined, Defined))
import "fay-base" Prelude
import "fay-base" Data.Text (Text, fromString)

import HaskellReact.Tag.Construct

type URL = Text
type Rel = Text
type HyperlinkTarget = Text

data HyperlinkAttributes = HyperlinkAttributes {
  href :: Defined URL
  , rel :: Defined Rel
  , target :: Defined HyperlinkTarget
}

author :: Rel
author = "author"

alternate :: Rel
alternate = "alternate"

bookmark :: Rel
bookmark = "bookmark"

help :: Rel
help = "help"

license :: Rel
license = "license"

next :: Rel
next = "next"

nofollow :: Rel
nofollow = "nofollow"

noreferrer :: Rel
noreferrer = "noreferrer"

prefetch :: Rel
prefetch = "prefetch"

prev :: Rel
prev = "prev"

search :: Rel
search = "search"

tag :: Rel
tag = "tag"

blank :: HyperlinkTarget
blank = "_blank"

parent :: HyperlinkTarget
parent = "_parent"

self :: HyperlinkTarget
self = "_self"

top :: HyperlinkTarget
top = "_top"

framename :: Text -> HyperlinkTarget
framename = Prelude.id

mkAAttrs :: HyperlinkAttributes
mkAAttrs = defaultHyperlinkAttributes

defaultHyperlinkAttributes :: HyperlinkAttributes
defaultHyperlinkAttributes = HyperlinkAttributes {
  href = Defined "javascript://"
  , rel = Undefined
  , target = Undefined
}

a''' :: (Renderable a) => Attributes -> a -> DOMElement
a''' attributes = a'' attributes mkAAttrs

a'' :: (Renderable a) => Attributes -> HyperlinkAttributes -> a -> DOMElement
a'' = constructDOMElement "a" 

a' :: (Renderable a) => HyperlinkAttributes -> a -> DOMElement
a' = a'' defaultAttributes

a :: (Renderable b) => URL -> b -> DOMElement
a url = a' (mkAAttrs { href = Defined url })
