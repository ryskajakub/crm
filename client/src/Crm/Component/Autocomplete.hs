{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Component.Autocomplete (
  autocompleteInput ) where

import HaskellReact
import FFI (ffi, Defined(Defined))
import "fay-base" Data.Text (pack, Text, (<>))
import "fay-base" Prelude hiding (id)

import qualified HaskellReact.Tag.Input as I

import Crm.Server (fetchMachineTypesAutocomplete)

data JQueryUI

data Request

data UIEvent

data UI

getTerm :: Request -> Text
getTerm = ffi " %1['term'] "

getUIValue :: UI -> Text
getUIValue = ffi " %1['item']['value'] "

data AutocompleteProps = AutocompleteProps {
  select :: UIEvent -> UI -> Fay () ,
  source :: Request -> ([Text] -> Fay ()) -> Fay () }

jQueryUI :: JQueryUI
jQueryUI = ffi " (function () { var $ = require('jquery'); require('jquery-ui'); return $; })() "

jQueryUIAutocomplete :: JQueryUI 
                     -> Text 
                     -> AutocompleteProps 
                     -> Fay ()
jQueryUIAutocomplete = ffi " %1(%2).autocomplete(%3) "

autocompleteInput :: Attributes -- ^ attributes to the input field
                  -> (Text -> Fay ()) -- ^ handler for the on change event
                  -> (Text -> Fay ()) -- ^ handler for the on select event
                  -> Text -- ^ id of the element
                  -> I.InputAttributes
                  -> (DOMElement, Fay ())
autocompleteInput attrs onChange onSelect elementId inputAttrs = let 
  onSelect' _ ui = onSelect (getUIValue ui)
  element = I.input
    (attrs { id = Defined elementId })
    (inputAttrs {
      I.onChange = Defined $ eventValue >=> onChange })
  autocomplete = jQueryUIAutocomplete 
    jQueryUI 
    (pack "#" <> elementId)
    (AutocompleteProps { 
      select = onSelect' , 
      source = \request response -> do
        let term = getTerm request
        fetchMachineTypesAutocomplete term response })
  in (element, autocomplete)
