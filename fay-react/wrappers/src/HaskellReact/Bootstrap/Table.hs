{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HaskellReact.Bootstrap.Table (
  TableStyle(..) ,
  table' ,
  table ) where

import "fay-base" Data.Text (fromString, Text)
import "fay-base" Prelude

import qualified HaskellReact as HR
import qualified HaskellReact.Tag.Table as T


data TableStyle = Bordered | FullBordered

styleAsClass :: TableStyle -> Text
styleAsClass Bordered = "bordered"
styleAsClass FullBordered = "table-bordered"


table' :: 
  HR.Renderable a =>
  HR.Attributes ->
  Maybe TableStyle ->
  a ->
  HR.DOMElement
table' attrs tableStyle' contents = let 
  tableClasses = ["table"] ++ (maybe [] ((:[]) . styleAsClass) tableStyle')
  in T.table' (HR.addClasses attrs tableClasses) contents


table :: 
  HR.Renderable a =>
  Maybe TableStyle ->
  a ->
  HR.DOMElement
table = table' HR.mkAttrs
