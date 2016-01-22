{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HaskellReact.Tag.Table (
  TableCellAttributes (..) ,
  mkTableCellAttributes ,
  tr , tr' ,
  table , table' ,
  thead , thead' ,
  tbody , tbody' ,
  th , th' ,
  td , td' , td'' ,
  colgroup' , colgroup ,
  col' , col
  ) where

import "fay-base" Prelude hiding (div, span)
import "fay-base" Data.Text (Text, fromString)
import            FFI (Defined(Undefined))

import HaskellReact.Tag.Construct
import HaskellReact.Tag.Simple

data TableCellAttributes = TableCellAttributes {
  colSpan :: Defined Int ,
  rowSpan :: Defined Int }

mkTableCellAttributes :: TableCellAttributes
mkTableCellAttributes = TableCellAttributes Undefined Undefined

th :: (Renderable a) => a -> DOMElement
th = th' defaultAttributes

th' :: (Renderable a) => Attributes -> a -> DOMElement
th' = constructSimple "th"

tr :: (Renderable a) => a -> DOMElement
tr = tr' defaultAttributes

tr' :: (Renderable a) => Attributes -> a -> DOMElement
tr' = constructSimple "tr"

td'' :: Renderable a => Attributes -> TableCellAttributes -> a -> DOMElement
td'' = constructDOMElement "td"

td' :: (Renderable a) => Attributes -> a -> DOMElement
td' = constructSimple "td"

td :: (Renderable a) => a -> DOMElement
td = td' defaultAttributes

table :: Renderable a => a -> DOMElement
table = table' defaultAttributes

table' :: Renderable a => Attributes -> a -> DOMElement
table' = constructSimple "table"

thead :: (Renderable a) => a -> DOMElement
thead = thead' defaultAttributes

thead' :: (Renderable a) => Attributes -> a -> DOMElement
thead' = constructSimple "thead"

tbody :: (Renderable a) => a -> DOMElement
tbody = tbody' defaultAttributes

tbody' :: (Renderable a) => Attributes -> a -> DOMElement
tbody' = constructSimple "tbody"

colgroup' :: (Renderable a) => Attributes -> a -> DOMElement
colgroup' = constructSimple "colgroup"

colgroup :: (Renderable a) => a -> DOMElement
colgroup = colgroup' defaultAttributes

col' :: (Renderable a) => Attributes -> a -> DOMElement
col' = constructSimple "col"

col :: (Renderable a) => a -> DOMElement
col = col' defaultAttributes
