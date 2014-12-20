{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Other where

import FFI
import "fay-base" Prelude

arg :: Int -> Int -> Fay ()
arg a b = putStrLn $ show $ a + b
