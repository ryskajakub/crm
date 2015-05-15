{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Component.Viz where

import FFI
import "fay-base" Data.Text (fromString, Text)

viz :: Text -> Text
viz = ffi " Viz(%1,\"svg\",\"dot\", null) " 
