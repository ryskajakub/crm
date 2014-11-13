{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
module Hello where

import FFI

data ReactClass

class Renderable a

data ReactInstance
instance Renderable ReactInstance

data DOMElement
instance Renderable DOMElement
instance Renderable String

data Attributes = Attributes { className :: String }

declareReactClass :: DOMElement -> ReactClass
declareReactClass = ffi " declareReactClass(%1) "

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
  let span = constructDOMElement "span" "JAJ"
  let label = constructDOMElement "label" "DDD"
  let div = constructDOMElementArray "div" [span, label]
  let clazz = declareReactClass div
  let inst = classInstance clazz (Attributes "blue")
  placeInstance inst
