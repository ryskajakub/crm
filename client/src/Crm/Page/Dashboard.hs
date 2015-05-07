{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Page.Dashboard (
  dashboard ) where

import "fay-base" Data.Text (fromString) 
import "fay-base" Prelude hiding (div, span, id)

import HaskellReact

dashboard :: DOMElement
dashboard = span "dashboard"
