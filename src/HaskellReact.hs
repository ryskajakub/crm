{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports #-}

module HaskellReact where

import FFI
import "fay-base" Data.Text (Text, append, showInt, pack)
import "fay-base" Data.Maybe (fromMaybe)

data DOMElement
data ReactClass

data SetState a

data ReactData a = ReactData {
  render :: (a, SetState a) -> DOMElement
  , componentWillMount :: Fay()
  , componentDidMount :: Fay()
  , displayName :: String
  , getInitialState :: a
}

defaultReactData :: a -> ReactData a 
defaultReactData initialState = ReactData {
  render = const $ constructDOMElement "div" defaultAttributes (pack "")
  , componentWillMount = return ()
  , componentDidMount = return ()
  , displayName = "<HaskellReactClass>"
  , getInitialState = initialState
}

data Attributes = Attributes {
  className :: String
  , onClick :: SyntheticMouseEvent -> Fay()
}

defaultAttributes = Attributes {
  className = ""
  , onClick = const $ return ()
}

data SyntheticMouseEvent

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

getType :: SyntheticMouseEvent -> String
getType = ffi " %1['type'] "
