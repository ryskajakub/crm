{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HaskellReact.Tag.Option (option) where

import "fay-base" Data.Text (Text, fromString)
import HaskellReact.Tag.Construct

data OptionAttrs = OptionAttrs {
  value_ :: Text
}

option :: Text -> Text -> DOMElement
option value name = constructDOMElement "option" defaultAttributes ( OptionAttrs { value_ = value } ) name
