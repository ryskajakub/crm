{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports #-}

module HaskellReact where

import FFI
import "fay-base" Data.Text (Text, append, showInt, pack)
import "fay-base" Data.Maybe (fromMaybe)
import Prelude hiding (id)

type URL = Text
type Rel = Text
type Target = Text

data AAttributes = AAttributes {
  href :: Maybe URL
  , rel :: Maybe Rel
  , target :: Maybe Target
}

aAttributesDefaults :: AAttributes
aAttributesDefaults = AAttributes Nothing Nothing Nothing

a :: (Renderable x) => Attributes -> AAttributes -> x -> DOMElement
a = ffi " constructDOMElement(\"a\", %1, Fay$$_(%3), %2) "

data ReactClass
data ReactInstance a
data SyntheticMouseEvent
data DOMElement

class Renderable a

instance Renderable Text
instance Renderable DOMElement

constructDOMElement :: (Renderable a) => String -> Attributes -> a -> DOMElement
constructDOMElement = ffi " constructDOMElement(%1, %2, Fay$$_(%3)) "

constructDOMElementArray :: String -> Attributes -> [DOMElement] -> DOMElement
constructDOMElementArray = ffi "constructDOMElement(%*)"

data ReactData a = ReactData {
  render :: ReactInstance a -> DOMElement
  , componentWillMount :: Fay()
  , componentDidMount :: Fay()
  , componentWillUnmount :: Fay()
  , displayName :: String
  , getInitialState :: a
}

defaultReactData :: a -> ReactData a
defaultReactData initialState = ReactData {
  render = const $ constructDOMElement "div" defaultAttributes (pack "")
  , componentWillMount = return ()
  , componentDidMount = return ()
  , componentWillUnmount = return ()
  , displayName = "<HaskellReactClass>"
  , getInitialState = initialState
}

data Attributes = Attributes {
  className :: String
  , onClick :: SyntheticMouseEvent -> Fay()
  , id :: String
}

defaultAttributes = Attributes {
  className = ""
  , onClick = const $ return ()
  , id = ""
}

declareReactClass :: ReactData a -> ReactClass
declareReactClass = ffi " declareReactClass(%1) "

setState :: ReactInstance a -> a -> Fay()
setState = ffi " %1['setState'](Fay$$_(%2)) "

state :: ReactInstance a -> a
state = ffi " %1['state'] "

isMounted :: ReactInstance a -> Bool
isMounted = ffi " %1['isMounted']() "

classInstance :: ReactClass -> DOMElement
classInstance = ffi " %1(null) "

placeElement :: DOMElement -> Fay ()
placeElement = ffi " renderReact(%1) "

getType :: SyntheticMouseEvent -> String
getType = ffi " %1['type'] "
