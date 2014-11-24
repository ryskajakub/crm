{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports #-}

module HaskellReact (
  module HaskellReact.Tag.Construct
  , module HaskellReact.Tag.Simple
  , module HaskellReact.Event
  , module HaskellReact.Component
  , module HaskellReact.ComponentData
  , module HaskellReact.ReadFay
  , textElement
  , phantom
  , toDOM
) where 

import FFI
import "fay-base" Data.Text (pack)
import HaskellReact.Event
import HaskellReact.Tag.Construct
import HaskellReact.Tag.Simple
import HaskellReact.Component
import HaskellReact.ComponentData
import HaskellReact.ReadFay

phantom :: a -> b
phantom = ffi " %1 "

textElement :: String -> DOMElement
textElement = phantom . pack

toDOM :: ReactInstance -> DOMElement
toDOM = phantom
