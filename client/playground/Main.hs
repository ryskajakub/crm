{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import "fay-base" Prelude hiding (span, div, elem)
#ifndef FAY
import qualified "base" Prelude as BasePrelude
#endif

import HaskellReact hiding (main)
import Playground

#ifdef FAY
main :: Fay ()
main = renderLoop
#else
main :: BasePrelude.IO ()
main = undefined
#endif
