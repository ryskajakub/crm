{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP #-}

module Main where

import "fay-base" Prelude hiding (span, div, elem)
#ifndef FAY
import qualified "base" Prelude as BasePrelude
#endif
import "fay-base" Data.Var (Var, newVar, subscribeAndRead, get, modify, waitFor)
import "fay-base" FFI (ffi, Nullable)
import "fay-base" Data.Text (Text, pack)
import "fay-base" Data.Maybe (isJust)

main' :: Fay ()
main' = putStrLn "ahoj"

#ifdef FAY
main :: Fay ()
main = main'
#else
main :: BasePrelude.IO ()
main = undefined
#endif
