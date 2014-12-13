{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HaskellReact.Bootstrap.Input where

import "fay-base" Prelude
import "fay-base" Data.Text (Text, fromString)
import FFI (Defined(Undefined, Defined), Nullable(Null))
import HaskellReact
import HaskellReact.Bootstrap

data InputProps = InputProps {
  type_ :: Text
}

mkInputProps :: InputProps
mkInputProps = InputProps ("text")

input :: InputProps -> DOMElement
input inputProps = reactInstance2DOM $ reactBootstrap "Input" inputProps ([]::[DOMElement])
