{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Crm.Component.CalendarInput as CI

main' :: Fay ()
main' = simpleReactBody element

element :: DOMElement
element = div $ CI.calendarInput

#ifdef FAY
main :: Fay ()
main = main'
#else
main :: BasePrelude.IO ()
main = undefined
#endif
