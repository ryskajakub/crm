{-# LANGUAGE PackageImports #-}

module HaskellReactSpec where

import FFI (Defined(Defined))
import HaskellReact
import "fay-base" Data.Text (Text, append, showInt, pack, unpack)
import Prelude hiding (span)
import Tag.Input (input, defaultInputAttributes, onChange)

data SimpleState = SimpleState { number :: Int }

data ReactState = ReactState {
  header1 :: Text
  , countClicks :: Int
}

singleElement :: DOMElement
singleElement = let
  innerData = ReactState (pack "The header") 0
  reactData = (defaultReactData innerData) {
    render = \reactInstance -> do
      data' <- state reactInstance
      let text = (header1 data') `append` (pack " ") `append` (showInt $ countClicks data')
      let onClick = const $ setState reactInstance (data' { countClicks = countClicks data' + 1} )
      let attrs = defaultAttributes { onClick = Defined onClick }
      return $ constructDOMElement "a" attrs text
    , displayName = "SpanClass"
    , getInitialState = innerData
  }
  in classInstance (declareReactClass reactData)

element :: DOMElement
element = let
  innerData = ReactState (pack "Element") 0
  reactData = (defaultReactData innerData) {
    render = \reactInstance -> do
      mounted <- isMounted reactInstance
      return $ constructDOMElement "h1" defaultAttributes (pack $ show mounted)
  }
  in classInstance (declareReactClass reactData)

aElement :: DOMElement
aElement = let
  innerData = ReactState (pack "AElement") 0
  reactData = (defaultReactData innerData) {
    render = \reactInstance -> return $ let
      aAttr = aAttributesDefaults {
        href = Defined $ pack $ "http://google.com"
      }
      in a defaultAttributes aAttr (pack "Google")
  }
  in classInstance (declareReactClass reactData)

relatedElements :: DOMElement
relatedElements = let
  reactData = (defaultReactData (SimpleState 0)) {
    render = \reactInstance -> do
      actualState <- state reactInstance
      let spanElement = span (pack "Num: " `append` (pack $ show $ number actualState))
          inputElement = input defaultAttributes (defaultInputAttributes {
            onChange = Defined $ \changeEvent -> do
              value <- eventValue changeEvent
              let ss = SimpleState $ length value
              (setState reactInstance ss)
          }) (pack "")
          divElement = constructDOMElementArray "div" defaultAttributes [spanElement, inputElement]
      return divElement
    }
  in declareAndRun reactData
