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
newtype Rel = Rel Text
newtype HyperlinkTarget = HyperlinkTarget Text

data HyperlinkAttributes = HyperlinkAttributes {
  href :: Defined URL
  , rel :: Defined Rel
  , target :: Defined HyperlinkTarget
}

author :: Rel
author = Rel "author"

alternate :: Rel
alternate = Rel "alternate"

bookmark :: Rel
bookmark = Rel "bookmark"

help :: Rel
help = Rel "help"

license :: Rel
license = Rel "license"

next :: Rel
next = Rel "next"

nofollow :: Rel
nofollow = Rel "nofollow"

noreferrer :: Rel
noreferrer = Rel "noreferrer"

prefetch :: Rel
prefetch = Rel "prefetch"

prev :: Rel
prev = Rel "prev"

search :: Rel
search = Rel "search"

tag :: Rel
tag = Rel "tag"

blank :: HyperlinkTarget
blank = HyperlinkTarget "_blank"

parent :: HyperlinkTarget
parent = HyperlinkTarget "_parent"

self :: HyperlinkTarget
self = HyperlinkTarget "_self"

top :: HyperlinkTarget
top = HyperlinkTarget "_top"

framename :: Text -> HyperlinkTarget
framename = HyperlinkTarget

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
