{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Sample where

import FFI
import Fay.Text

data DOMElement
data ReactClass

data InnerData = InnerData {
  companyName :: Text
  , anything :: Int
}
data SetState a

data ReactData a = ReactData {
  render :: (a, SetState a) -> DOMElement
  , componentDidMount :: Fay()
  , displayName :: String
  , getInitialState :: a
}

data Attributes = Attributes {
  className :: String
  , onClick :: Fay()
}

declareReactClass :: ReactData a -> ReactClass
declareReactClass = ffi " declareReactClass(%1) "

setState :: SetState a -> a -> Fay()
setState = ffi " %1(Fay$$_(%2)) "

class Renderable a

instance Renderable Text
instance Renderable DOMElement

constructDOMElement :: (Renderable a) => String -> Attributes -> a -> DOMElement
constructDOMElement = ffi " constructDOMElement(%1, %2, Fay$$_(%3)) "

constructDOMElementArray :: String -> Attributes -> [DOMElement] -> DOMElement
constructDOMElementArray = ffi "constructDOMElement(%*)"

classInstance :: ReactClass -> DOMElement
classInstance = ffi " %1(null) "

placeElement :: DOMElement -> Fay ()
placeElement = ffi " renderReact(%1) "

render' :: (InnerData, SetState InnerData) -> DOMElement
render' (d, ss) = let
  text = companyName d
  click2 = setState ss (InnerData (pack "AAAAAAAAAAAA") 5)
  in constructDOMElement "span" (Attributes "blue" click2) text

attr :: Attributes
attr = Attributes "" (return ())

data DifferentInnerData = DifferentInnerData {
  header :: Text
}

differentClass :: DOMElement
differentClass = let
  dd = DifferentInnerData $ pack "Big header"
  attr ss = Attributes "" (setState ss (DifferentInnerData "BBBB"))
  data' = ReactData {
    render = \(state, ss) -> constructDOMElement "h1" (attr ss) (header state)
    , componentDidMount = return ()
    , displayName = "SpanClass2"
    , getInitialState = dd
  }
  element = classInstance (declareReactClass data')
  in element

main :: Fay ()
main = do
  let
    afterMount = putStrLn("component did mount!!!")
    innerData = InnerData (pack "Firma1") 8
    reactData = ReactData {
      render = render'
      , componentDidMount = afterMount
      , displayName = "SpanClass"
      , getInitialState = innerData
    }
    spanClass = classInstance (declareReactClass reactData)
  placeElement (constructDOMElementArray "div" (Attributes "blue" (return ())) [spanClass, differentClass])
