{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HaskellReact.React (
  CommonJSModule
  , foreignReact
  , simpleReact, simpleReactBody
) where

import FFI (ffi, Automatic)
import HaskellReact.Tag.Construct
import "fay-base" Prelude
import "fay-base" Data.Text (Text)
import DOM (Element, getBody)
import "fay-base" Data.Function (fmap)

class CommonJSModule a

foreignReact :: (CommonJSModule b, Renderable c)
             => Automatic b -- ^ module imported with CommonJS's @require()@
             -> Text -- ^ name of the property in the module
             -> Automatic a -- ^ props passed to the React class
             -> Automatic c -- ^ children passed to the React class
             -> DOMElement
foreignReact = ffi "\
  \ (function () {\
    \ var attributes = %3;\
    \ var escapedAttributes = {};\
    \ for (key in attributes) {\
      \ var newKey = (key.charAt(key.length - 1) == '_' ? key.substring(0, key.length - 1) : key);\
      \ escapedAttributes[newKey] = attributes[key];\
    \ }\
    \ return (require('react')['createElement'])(%1[%2], escapedAttributes, %4);\
  \ })()\
\ "

simpleReactBody' :: DOMElement
                 -> Fay ()
                 -> Fay ()
simpleReactBody' elementToRender callbacks = do
  body <- getBody
  simpleReact elementToRender body callbacks

simpleReactBody :: DOMElement
                -> Fay ()
simpleReactBody element = simpleReactBody' element (return ())

simpleReact :: DOMElement -- ^ element to render
            -> Element -- ^ point in document where to place the element
            -> Fay () -- ^ callback to call after the virtual dom is rendered in the browser
            -> Fay ()
simpleReact = ffi " require('react').render(%1, %2, %3) "
