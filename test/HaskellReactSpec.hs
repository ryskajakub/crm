{-# LANGUAGE PackageImports #-}

module HaskellReactSpec where

import HaskellReact
import "fay-base" Data.Text (Text, append, showInt, pack, unpack)
import "fay-base" Data.Maybe (fromMaybe)
import Prelude hiding (span)

data ReactState = ReactState {
  header1 :: Text
  , countClicks :: Int
}

data SimpleState = SimpleState { number :: Int }

render' :: ReactInstance ReactState -> DOMElement
render' reactInstance = let
  data' = state reactInstance
  text = (header1 data') `append` (pack " ") `append` (showInt $ countClicks data')
  onClick = const $ setState reactInstance (data' { countClicks = countClicks data' + 1} )
  in constructDOMElement "a" (defaultAttributes { className = Just "blue", onClick = Just onClick }) text

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

element :: DOMElement
element = let
  innerData = ReactState (pack "Element") 0
  reactData = (defaultReactData innerData) {
    render = \reactInstance -> let
      mounted = isMounted reactInstance
      in constructDOMElement "h1" defaultAttributes (pack $ show mounted)
  }
  in classInstance (declareReactClass reactData)

aElement :: DOMElement
aElement = let
  innerData = ReactState (pack "AElement") 0
  reactData = (defaultReactData innerData) {
    render = \reactInstance -> let
      aAttr = aAttributesDefaults {
        href = Just $ pack $ "http://google.com"
      }
      in a defaultAttributes aAttr (pack "Google")
  }
  in classInstance (declareReactClass reactData)

relatedElements :: DOMElement
relatedElements = let
  reactData = (defaultReactData (SimpleState 0)) {
    render = \reactInstance -> let
      spanElement = span (pack "Num: " `append` (pack $ show $ number $ state reactInstance))
      inputElement = input defaultAttributes (defaultInputAttributes {
        onChange = Just $ \changeEvent -> do
          value <- eventValue changeEvent
          let ss = SimpleState $ length value
          (setState reactInstance ss)
      }) (pack "")
      divElement = constructDOMElementArray "div" defaultAttributes [spanElement, inputElement]
      in divElement }
  in declareAndRun reactData
