{-# LANGUAGE PackageImports #-}

module Main where

import FFI
import "fay-base" Data.Text (Text, append, showInt, pack)
import "fay-base" Data.Maybe (fromMaybe)
import HaskellReact
import Tag.Input

data InnerData = InnerData {
  header :: Text
}

main :: Fay ()
main = placeElement $ declareAndRun $ (defaultReactData (InnerData $ pack "AHOJ")) {
  render = \reactInstance ->
    return $ input defaultAttributes (defaultInputAttributes {
      onChange = Just $ \changeEvent -> eventValue changeEvent >>= putStrLn
    }) (pack "")
}
