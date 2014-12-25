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

data JQueryUI

data Request

getTerm :: Request -> Text
getTerm = ffi " %1['term'] "

data Choice = Choice {
  label_ :: Text ,
  value_ :: Text }

data AutocompleteProps = AutocompleteProps {
  source :: Request -> ([Choice] -> Fay ()) -> Fay () }

escape :: Choice -> Choice
escape = ffi " (function (attributes) { delete attributes['instance']; var escapedAttributes = {}; for (key in attributes) { var newKey = (key.charAt(key.length - 1) == '_' ? key.substring(0, key.length - 1) : key); escapedAttributes[newKey] = attributes[key]; } return escapedAttributes; })(%1) "

escape' :: [Choice] -> [Choice]
escape' list = map escape list

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
    (AutocompleteProps (\request response -> let
      term = getTerm request
      in response $ escape' (Choice term term : [Choice (pack "aaaaaaaaaaa") (pack "bbbb")]) ))
  in (element, autocomplete)
