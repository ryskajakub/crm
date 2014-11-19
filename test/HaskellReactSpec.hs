{-# LANGUAGE PackageImports #-}

module HaskellReactSpec where

import FFI (Defined(Defined), ffi)
import HaskellReact
import "fay-base" Data.Text (Text, append, showInt, pack)
import Prelude hiding (span, div)
import HaskellReact.Tag.Input (input, defaultInputAttributes, onChange)
import HaskellReact.Tag.Hyperlink (a, href, aAttr)

data SimpleState = SimpleState { number :: Int }

data ReactState = ReactState {
  header1 :: Text
  , countClicks :: Int
}

render' :: ReactThis ReactState -> Fay DOMElement
render' = \reactInstance -> do
  data' <- state reactInstance
  let text = (header1 data') `append` (pack " ") `append` (showInt $ countClicks data')
  let onClickHandler = Defined $ const $ setState reactInstance (data' { countClicks = countClicks data' + 1} )
  return $ constructDOMElement "a" (defaultAttributes { className = Defined "blue", onClick = onClickHandler }) defaultAttributes text

singleElement :: ReactInstance
singleElement = let
  innerData = ReactState (pack "The header") 0
  reactData = (defaultReactData innerData) {
    render = render'
    , displayName = "SpanClass"
  }
  in declareAndRun reactData

element :: ReactInstance
element = let
  innerData = ReactState (pack "Element") 0
  reactData = (defaultReactData innerData) {
    render = \reactInstance -> do
      mounted <- isMounted reactInstance
      return $ constructDOMElement "h1" defaultAttributes defaultAttributes (pack $ show mounted)
  }
  in classInstance (declareReactClass reactData)

aElement :: ReactInstance
aElement = let
  innerData = ReactState (pack "AElement") 0
  reactData = (defaultReactData innerData) {
    render = const $ return $ 
      a (aAttr {href = Defined $ pack $ "http://google.com"}) (pack "Google")
  }
  in classInstance (declareReactClass reactData)

relatedElements :: ReactInstance
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
          divElement = div [spanElement, inputElement]
      return divElement
    }
  in declareAndRun reactData

main :: Fay ()
main =  ffi " (function() { var obj = {}; obj['HaskellReactSpec'] = HaskellReactSpec; obj['FayDD'] = Fay$$_; module.exports = obj; })() "
