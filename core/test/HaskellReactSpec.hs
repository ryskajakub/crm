{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellReactSpec where

import FFI (Defined(Defined), ffi)
import HaskellReact
import "fay-base" Data.Text (Text, append, showInt, pack, fromString, length)
import Prelude hiding (span, div, length)
import HaskellReact.Tag.Input (input, defaultInputAttributes, onChange)
import HaskellReact.Tag.Hyperlink (a, href, aAttr)
import HaskellReact.ReadFay (RF(RF), rf, readFayReturn, ReadFay, runReadFay, state, isMounted)

data SimpleState = SimpleState { number :: Int }

data ReactState = ReactState {
  header1 :: Text
  , countClicks :: Int
}

render' :: ReactThis ReactState b -> ReadFay DOMElement
render' = \reactInstance -> let RF return (>>=) _ = rf in do
  data' <- state reactInstance
  let text = (header1 data') `append` (" ") `append` (showInt $ countClicks data')
  let onClickHandler = Defined $ const $ setState reactInstance (data' { countClicks = countClicks data' + 1} )
  readFayReturn $ constructDOMElement "a" (defaultAttributes { className = Defined "blue", onClick = onClickHandler }) defaultAttributes text

singleElement :: ReactInstance
singleElement = let
  innerData = ReactState ("The header") 0
  reactData' = (reactData ("SingleElement") innerData) (render')
  in declareAndRun reactData'

element :: ReactInstance
element = let
  innerData = ReactState ("Element") 0
  reactData' = (reactData ("Element") innerData) (
    \reactInstance -> let RF return (>>=) _ = rf in do
      mounted <- isMounted reactInstance
      readFayReturn $ constructDOMElement "h1" defaultAttributes defaultAttributes (pack $ show mounted)
    )
  in classInstance (declareReactClass reactData')

aElement :: ReactInstance
aElement = let
  innerData = ReactState ("AElement") 0
  reactData' = (reactData ("Anchor1") innerData) (
    const $ readFayReturn $ a (aAttr {href = Defined "http://google.com"}) ("Google")
    )
  in classInstance (declareReactClass reactData')

onChange' :: ReactThis SimpleState b -> SyntheticEvent -> Fay ()
onChange' reactInstance changeEvent = do
  value <- eventValue changeEvent
  let ss = SimpleState $ length value
  (setState reactInstance ss)

relatedElements :: ReactInstance
relatedElements = let
  reactData' = (reactData ("Simple1") (SimpleState 0)) (
    \reactInstance -> let RF return (>>=) _ = rf in do
      actualState <- state reactInstance
      let spanElement = span ("Num: " `append` (pack $ show $ number actualState))
          inputElement = input defaultAttributes (defaultInputAttributes {
            onChange = Defined $ \changeEvent -> onChange' reactInstance changeEvent
          }) ("")
          divElement = div [spanElement, inputElement]
      return divElement
      )
  in declareAndRun reactData'

main :: Fay ()
main =  ffi " (function() { var obj = {}; obj['HaskellReactSpec'] = HaskellReactSpec; obj['FayDD'] = Fay$$_; module.exports = obj; })() "
