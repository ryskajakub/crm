{-# LANGUAGE PackageImports #-}

module Main where

import FFI
import "fay-base" Data.Text (Text, append, showInt, pack)
import "fay-base" Data.Maybe (fromMaybe)
import HaskellReact

data InnerData = InnerData {
  header :: Text
}

main :: Fay ()
main = placeElement $ declareAndRun $ (defaultReactData (InnerData $ pack "AHOJ")) {
  render = \reactInstance ->
    input defaultAttributes (defaultInputAttributes {
      onChange = Just $ \changeEvent -> putStrLn $ eventValue changeEvent }) (pack "")
}
