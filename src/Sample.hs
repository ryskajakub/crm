{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
module Hello where

import FFI
import Fay.Text

data DOMElement
data ReactClass

data InnerData = InnerData {
  employees :: [String]
  , companyName :: String
}
data SetState

data ReactData = ReactData {
  render :: (InnerData, SetState) -> DOMElement
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

class Renderable a

setState :: SetState -> InnerData -> Fay()
setState = ffi " %1(%2) "

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

render' :: (InnerData, SetState) -> DOMElement
render' (d, ss) = let
  text = pack $ (companyName d)
  click2 = setState ss (InnerData [] "AAAAAAAAAAAA")
  e = constructDOMElement "span" (Attributes "nothing" click2) text
  in constructDOMElement "span" (Attributes "blue" clickHandler) e

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
    spanClass = classInstance (declareReactClass reactData)
  placeElement spanClass
