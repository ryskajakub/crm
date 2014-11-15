{-# LANGUAGE PackageImports #-}

module Main where

import FFI
import "fay-base" Data.Text (Text, append, showInt, pack)
import "fay-base" Data.Maybe (fromMaybe)
import HaskellReact

data InnerData = InnerData {
  companyName :: Text
  , anything :: Int
}

data DifferentInnerData = DifferentInnerData {
  header :: Maybe Text
}

differentClass :: DOMElement
differentClass = let
  dd = DifferentInnerData $ Just $ pack "Big header"
  attr reactInstance = Attributes { onClick = (\event -> do
    let type' = getType event
    putStrLn type'
    setState reactInstance (DifferentInnerData $ Just $ pack type')
    ) }
  data' = (defaultReactData dd) {
    render = \(reactInstance) -> constructDOMElement "h1" (attr reactInstance) (fromMaybe (pack "default") (header $ state reactInstance))
    , componentDidMount = return ()
    , displayName = "SpanClass2"
  }
  element = classInstance (declareReactClass data')
  in element

main :: Fay ()
main = placeElement (constructDOMElement "div" (defaultAttributes { className = "blue" }) differentClass)
