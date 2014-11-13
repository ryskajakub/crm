{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
module Hello where

import FFI

data ReactClass

data ReactInstance

data DOMElement

data Attributes = Attributes { className :: String }

declareReactClass :: DOMElement -> ReactClass
declareReactClass = ffi " declareReactClass(%1) "

declareReactClass' :: ReactInstance -> ReactClass
declareReactClass' = ffi " declareReactClass(%1) "

constructDOMElement :: String -> String -> DOMElement
constructDOMElement = ffi " constructDOMElement(%1, %2) "

constructDOMElementWithChildren :: String -> DOMElement -> DOMElement
constructDOMElementWithChildren = ffi "constructDOMElement(%1, %2)"

constructDOMElementArray :: String -> [DOMElement] -> DOMElement
constructDOMElementArray = ffi "constructDOMElement(%1, %2)"

classInstance :: ReactClass -> Attributes -> ReactInstance
classInstance = ffi " %1(%2) "

placeInstance :: ReactInstance -> Fay ()
placeInstance = ffi " renderReact(%1) "

main :: Fay ()
main = do
  let inputField = classInstance (declareReactClass $ constructDOMElement "input" "") (Attributes "red")
  let span = constructDOMElement "span" "JAJ"
  let label = constructDOMElement "label" "DDD"
  let div = constructDOMElementArray "div" [span, label]
  let clazz = declareReactClass' inputField
  let inst = classInstance clazz (Attributes "blue")
  placeInstance inst
