{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HaskellReact.Bootstrap.Glyphicon where

import "fay-base" Data.Text (Text, fromString, (<>))
import "fay-base" Prelude

import HaskellReact

glyphicon :: Text -> DOMElement
glyphicon glyphIdentificator = span'
  (class'' ["glyphicon", "glyphicon-" <> glyphIdentificator]) 
  ([]::[DOMElement])

plus :: DOMElement
plus = glyphicon "plus"

list :: DOMElement
list = glyphicon "list"

pencil :: DOMElement
pencil = glyphicon "pencil"

wrench :: DOMElement
wrench = glyphicon "wrench"

tasks :: DOMElement
tasks = glyphicon "tasks"

thList :: DOMElement
thList = glyphicon "th-list"

home :: DOMElement
home = glyphicon "home"
