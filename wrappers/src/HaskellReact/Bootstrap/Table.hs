{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HaskellReact.Bootstrap.Table (
  TableStyle(..) ,
  table ) where

import "fay-base" Data.Text (fromString, Text)
import "fay-base" Prelude

import qualified HaskellReact as HR
import qualified HaskellReact.Tag.Table as T
import HaskellReact.Bootstrap (reactBootstrap)


data TableStyle = Bordered

styleAsClass :: TableStyle -> Text
styleAsClass Bordered = "bordered"


table :: 
  HR.Renderable a =>
  Maybe TableStyle ->
  a ->
  HR.DOMElement
table tableStyle' contents = let 
  tableClasses = ["table"] ++ (maybe [] ((:[]) . styleAsClass) tableStyle')
  in T.table' (HR.class'' tableClasses) contents
