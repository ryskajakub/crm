{-# LANGUAGE PackageImports #-}

module HaskellReact.Tag.Hyperlink(
  a , a'
  , HyperlinkAttributes(HyperlinkAttributes)
  , href
  , rel
  , target
  , defaultHyperlinkAttributes
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

import FFI (Defined(Undefined))
import HaskellReact.Tag.Construct

type URL = String
newtype Rel = Rel String
newtype HyperlinkTarget = HyperlinkTarget String

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

framename :: String -> HyperlinkTarget
framename = HyperlinkTarget

defaultHyperlinkAttributes :: HyperlinkAttributes
defaultHyperlinkAttributes = HyperlinkAttributes {
  href = Undefined
  , rel = Undefined
  , target = Undefined
}

a' :: (Renderable a) => Attributes -> HyperlinkAttributes -> a -> DOMElement
a' = constructDOMElement "a" 

a :: (Renderable a) => HyperlinkAttributes -> a -> DOMElement
a = a' defaultAttributes
