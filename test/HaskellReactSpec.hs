{-# LANGUAGE PackageImports #-}

module HaskellReactSpec where

import HaskellReact
import "fay-base" Data.Text (Text, append, showInt, pack)
import "fay-base" Data.Maybe (fromMaybe)

data ReactState = ReactState {
  header1 :: Text
  , countClicks :: Int
}

render' :: (ReactState, SetState ReactState) -> DOMElement
render' (data', ss) = let
  text = (header1 data') `append` (pack " ") `append` (showInt $ countClicks data')
  onClick = setState ss (data' { countClicks = countClicks data' + 1} )
  in constructDOMElement "a" (Attributes "blue" onClick) text

singleElement :: DOMElement
singleElement = let
  innerData = ReactState (pack "The header") 0
  reactData = ReactData {
    render = render'
    , componentDidMount = return ()
    , displayName = "SpanClass"
    , getInitialState = innerData
  }
  in classInstance (declareReactClass reactData)
