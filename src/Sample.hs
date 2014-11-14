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
  , click :: Fay()
}

data ReactData = ReactData {
  render :: (InnerData, Props) -> DOMElement
  , componentDidMount :: Fay()
  , displayName :: String
  , getInitialState :: InnerData
}

data Attributes = Attributes {
  className :: String
  , onClick :: Fay()
}

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

render' :: (InnerData, Props) -> DOMElement
render' (d, p) = constructDOMElement "a" (Attributes "blue" clickHandler) ((companyName d) ++ [' '] ++ (color p))

clickHandler :: Fay()
clickHandler = putStrLn("clicked")

main :: Fay ()
main = do
  let
    afterMount = putStrLn("component did mount!!!")
    innerData = InnerData ["Karel", "Milan"] "Firma1"
    reactData = ReactData {
      render = render'
      , componentDidMount = afterMount
      , displayName = "SpanClass"
      , getInitialState = innerData
    }
    spanClass = classInstance (declareReactClass reactData) (Props "red" clickHandler)
  placeElement spanClass
