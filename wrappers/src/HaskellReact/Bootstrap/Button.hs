{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HaskellReact.Bootstrap.Button where

import HaskellReact (foreignReact, Renderable, CommonJSModule, ReactInstance, Empty (Empty))
import FFI (ffi, Automatic, Defined(Undefined), Nullable(Null))
import "fay-base" Data.Text (fromString, Text)
import HaskellReact.Event (SyntheticMouseEvent)
import "fay-base" Prelude
import HaskellReact.Bootstrap

data ButtonProps = ButtonProps {
  bsStyle :: Defined Text
  , title :: Defined Text
  , onClick :: Defined (SyntheticMouseEvent -> Fay ())
}

buttonProps :: ButtonProps
buttonProps = ButtonProps {
  bsStyle = Undefined
  , title = Undefined
  , onClick = Undefined
}

button' :: Renderable a
        => ButtonProps
        -> a
        -> ReactInstance
button' props children = reactBootstrap "Button" props children

button :: Renderable a
       => a
       -> ReactInstance
button = button' buttonProps
