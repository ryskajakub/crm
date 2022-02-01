{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HaskellReact.Event where

import "fay-base" Data.Text (Text, unpack, pack)
import "fay-base" FFI (ffi, Ptr)
import "fay-base" Prelude

data DOMEventTarget

data SyntheticEvent
data SyntheticMouseEvent
data SyntheticClipboardEvent
data SyntheticKeyboardEvent

class AnyEvent a
instance AnyEvent SyntheticMouseEvent
instance AnyEvent SyntheticEvent
instance AnyEvent SyntheticClipboardEvent
instance AnyEvent SyntheticKeyboardEvent

eventValue :: SyntheticEvent -> Fay Text
eventValue = ffi " %1['target']['value'] "

preventDefault :: (AnyEvent a) => Ptr a -> Fay ()
preventDefault = ffi " %1['preventDefault']() "

stopPropagation :: (AnyEvent a) => Ptr a -> Fay ()
stopPropagation = ffi " %1['stopPropagation']() "

target :: (AnyEvent a) => Ptr a -> Fay DOMEventTarget
target = ffi " %1['target'] "

getType :: SyntheticMouseEvent -> Fay Text
getType = ffi " %1['type'] "

eventString :: SyntheticEvent -> Fay String
eventString event = do
  text <- eventValue event
  return $ unpack text

clipboardData :: SyntheticClipboardEvent -> Fay Text
clipboardData = ffi " %1['clipboardData'].getData('text/plain') "

attribute :: (AnyEvent e) => Text -> e -> Fay a
attribute = ffi " %2[%1] " 

keyCode :: SyntheticKeyboardEvent -> Fay Int
keyCode = attribute (pack "keyCode")
