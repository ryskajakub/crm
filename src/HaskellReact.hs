{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports #-}

module HaskellReact where

import FFI
import Fay.Text
import "fay-base" Data.Maybe

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

attr :: Attributes
attr = Attributes "" (return ())

data DifferentInnerData = DifferentInnerData {
  header :: Maybe Text
}

differentClass :: DOMElement
differentClass = let
  dd = DifferentInnerData $ Just $ pack "Big header"
  attr ss = Attributes "" (setState ss (DifferentInnerData $ Nothing))
  data' = ReactData {
    render = \(state, ss) -> constructDOMElement "h1" (attr ss) (fromMaybe (pack "default") (header state))
    , componentDidMount = return ()
    , displayName = "SpanClass2"
    , getInitialState = dd
  }
  element = classInstance (declareReactClass data')
  in element

r :: (InnerData, SetState InnerData) -> DOMElement
r (innerData, ss) = let
  click = setState ss (InnerData (pack "abc") 88888)
  in constructDOMElement "h1" (Attributes "" click) ((companyName innerData) `append` (pack $ show $ anything innerData))

main :: Fay ()
main = do
  let
    innerData = InnerData (pack "Firma1") 8
    reactData = ReactData {
      render = r
      , componentDidMount = return ()
      , displayName = "SpanClass"
      , getInitialState = innerData
    }
    spanClass = classInstance (declareReactClass reactData)
  placeElement (constructDOMElementArray "div" (Attributes "blue" (return ())) [spanClass, differentClass])
