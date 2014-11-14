{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
module Hello where

import FFI

data DOMElement
data ReactClass

data InnerData = InnerData {
  employees :: [String]
  , companyName :: String
}

data Props = Props {
  color :: String
}

data ReactData = ReactData {
  render :: (InnerData, Props) -> DOMElement
  , componentDidMount :: Fay()
  , displayName :: String
  , getInitialState :: InnerData
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

classInstance :: ReactClass -> Props -> DOMElement
classInstance = ffi " %1(%2) "

placeElement :: DOMElement -> Fay ()
placeElement = ffi " renderReact(%1) "

main :: Fay ()
main = do
  let
    afterMount = putStrLn("component did mount!!!")
    span = constructDOMElement "span" (Attributes "blueish") "JAJ"
    innerData = InnerData ["Karel", "Milan"] "Firma1"
    reactData = ReactData {
      render = \(d, p) -> constructDOMElement "span" (Attributes "blue") ((companyName d) ++ [' '] ++ (color p))
      , componentDidMount = afterMount
      , displayName = "SpanClass"
      , getInitialState = innerData
    }
    spanClass = classInstance (declareReactClass reactData) (Props "red")
  placeElement spanClass
