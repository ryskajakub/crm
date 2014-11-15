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
  attr ss = Attributes "" (\event -> do
    let type' = getType event
    putStrLn type'
    setState ss (DifferentInnerData $ Just $ pack type')
    )
  data' = ReactData {
    render = \(state, ss) -> constructDOMElement "h1" (attr ss) (fromMaybe (pack "default") (header state))
    , componentDidMount = return ()
    , displayName = "SpanClass2"
    , getInitialState = dd
  }
  element = classInstance (declareReactClass data')
  in element

main :: Fay ()
main = placeElement (constructDOMElement "div" (Attributes "blue" (const $ return ())) differentClass)
