{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
module Hello where

import FFI

data DOMElement
data ReactClass

data ReactData = ReactData {
  render :: () -> DOMElement
  , displayName :: String
}

data Attributes = Attributes { className :: String }

declareReactClass :: ReactData -> ReactClass
declareReactClass = ffi " declareReactClass(%1) "

constructDOMElement :: String -> Attributes -> String -> DOMElement
constructDOMElement = ffi " constructDOMElement(%*) "

constructDOMElementWithChildren :: String -> Attributes -> DOMElement -> DOMElement
constructDOMElementWithChildren = ffi "constructDOMElement(%*)"

constructDOMElementArray :: String -> Attributes -> [DOMElement] -> DOMElement
constructDOMElementArray = ffi "constructDOMElement(%*)"

classInstance :: ReactClass -> Attributes -> DOMElement
classInstance = ffi " %1(%2) "

placeElement :: DOMElement -> Fay ()
placeElement = ffi " renderReact(%1) "

main :: Fay ()
main = do
  let span = constructDOMElement "span" (Attributes "blueish") "JAJ"
  let spanClass = classInstance (declareReactClass $ ReactData (const span) "SpanClass") (Attributes "red")
  placeElement spanClass
