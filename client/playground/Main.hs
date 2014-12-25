{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import "fay-base" Prelude hiding (span, div, elem)
#ifndef FAY
import qualified "base" Prelude as BasePrelude
#endif
import "fay-base" Data.Var (Var, newVar, subscribeAndRead, get, modify, waitFor)
import "fay-base" FFI (ffi, Nullable, Automatic)
import "fay-base" Data.Text (Text, pack, fromString)
import "fay-base" Data.Maybe (isJust)
import HaskellReact hiding (main)
import qualified HaskellReact.Tag.Input as I

#ifdef FAY
main :: Fay ()
main = let
  (element1, callback) = autocompleteInput
  in simpleReactBody' element1 callback
#else
main :: BasePrelude.IO ()
main = undefined
#endif
