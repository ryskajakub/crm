{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RebindableSyntax #-}

module HaskellReactSpec where

import FFI (Defined(Defined), ffi)
import HaskellReact
import "fay-base" Data.Text (Text, append, showInt, pack)
import Prelude hiding (span, div)
import HaskellReact.Tag.Input (input, defaultInputAttributes, onChange)
import HaskellReact.Tag.Hyperlink (a, href, aAttr)
import HaskellReact.ReadFay (RF(RF), rf, readFayReturn, ReadFay, runReadFay, state, isMounted)

data SimpleState = SimpleState { number :: Int }

data ReactState = ReactState {
  header1 :: Text
  , countClicks :: Int
}

render' :: ReactThis ReactState -> ReadFay DOMElement
render' = \reactInstance -> let RF return (>>=) _ = rf in do
  data' <- state reactInstance
  let text = (header1 data') `append` (pack " ") `append` (showInt $ countClicks data')
  let onClickHandler = Defined $ const $ setState reactInstance (data' { countClicks = countClicks data' + 1} )
  readFayReturn $ constructDOMElement "a" (defaultAttributes { className = Defined "blue", onClick = onClickHandler }) defaultAttributes text

singleElement :: ReactInstance
singleElement = let
  innerData = ReactState (pack "The header") 0
  reactData = (defaultReactData innerData) (render')
  in declareAndRun reactData

element :: ReactInstance
element = let
  innerData = ReactState (pack "Element") 0
  reactData = (defaultReactData innerData) (
    \reactInstance -> let RF return (>>=) _ = rf in do
      mounted <- isMounted reactInstance
      readFayReturn $ constructDOMElement "h1" defaultAttributes defaultAttributes (pack $ show mounted)
    )
  in classInstance (declareReactClass reactData)

aElement :: ReactInstance
aElement = let
  innerData = ReactState (pack "AElement") 0
  reactData = (defaultReactData innerData) (
    const $ readFayReturn $ a (aAttr {href = Defined $ pack "http://google.com"}) (pack "Google")
    )
  in classInstance (declareReactClass reactData)

onChange' :: ReactThis SimpleState -> SyntheticEvent -> Fay ()
onChange' reactInstance changeEvent = do
  value <- eventValue changeEvent
  let ss = SimpleState $ length value
  (setState reactInstance ss)

relatedElements :: ReactInstance
relatedElements = let
  reactData = (defaultReactData (SimpleState 0)) (
    \reactInstance -> let RF return (>>=) _ = rf in do
      actualState <- state reactInstance
      let spanElement = span (pack "Num: " `append` (pack $ show $ number actualState))
          inputElement = input defaultAttributes (defaultInputAttributes {
            onChange = Defined $ \changeEvent -> onChange' reactInstance changeEvent
          }) (pack "")
          divElement = div [spanElement, inputElement]
      return divElement
      )
  in declareAndRun reactData

main :: Fay ()
main =  ffi " (function() { var obj = {}; obj['HaskellReactSpec'] = HaskellReactSpec; obj['FayDD'] = Fay$$_; module.exports = obj; })() "
