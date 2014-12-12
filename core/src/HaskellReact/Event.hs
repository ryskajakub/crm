{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HaskellReact.Event where

import "fay-base" Data.Text (Text)
import FFI (ffi)
import "fay-base" Prelude

data SyntheticEvent
data SyntheticMouseEvent

eventValue :: SyntheticEvent -> Fay Text
eventValue = ffi " %1['target']['value'] "

getType :: SyntheticMouseEvent -> Fay Text
getType = ffi " %1['type'] "
