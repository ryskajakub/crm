{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP #-}

module Main where

import "fay-base" Prelude hiding (span, div, elem)
#ifndef FAY
import qualified "base" Prelude as BasePrelude
#endif
import "fay-base" Data.Var (Var, newVar, subscribeAndRead, get, modify, waitFor)
import "fay-base" FFI (ffi, Nullable)
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

data JQueryUI

data AutocompleteProps = AutocompleteProps {
  source :: [Text] }

jQueryUI :: JQueryUI
jQueryUI = ffi " (function () { var $ = require('jquery'); require('jquery-ui'); return $; })() "

jQueryUIAutocomplete :: JQueryUI 
                     -> Text 
                     -> AutocompleteProps 
                     -> Fay ()
jQueryUIAutocomplete = ffi " %1(%2).autocomplete(%3) "

autocompleteInput :: (DOMElement, Fay ())
autocompleteInput = let 
  element = I.input
    mkAttrs
    I.mkInputAttrs
  autocomplete = jQueryUIAutocomplete 
    jQueryUI 
    (pack "input")
    (AutocompleteProps [pack "abc", pack "def", pack "aaaaaaaaaaa"])
  in (element, autocomplete)
