{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Other (
  autocompleteInput ) where

import HaskellReact
import FFI (ffi)
import "fay-base" Data.Text (pack, Text)
import "fay-base" Prelude

import qualified HaskellReact.Tag.Input as I

