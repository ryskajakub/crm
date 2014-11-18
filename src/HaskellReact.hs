{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports #-}

module HaskellReact where

import FFI
import "fay-base" Data.Text (Text, pack)
import Prelude hiding (id, span)

type URL = Text
type Rel = Text
type Target = Text

data AAttributes = AAttributes {
  href :: Defined URL
  , rel :: Defined Rel
  , target :: Defined Target
}

class CommonJSModule a

foreignReactInstance :: (CommonJSModule b, Renderable c) 
                     => Automatic b -> String -> Automatic a -> Automatic c -> DOMElement
foreignReactInstance = ffi " %1[%2](%3, %4) "

foreignReactInstance' :: (CommonJSModule b, Renderable c) 
                     => Automatic b -> String -> Automatic a -> [Automatic c] -> DOMElement
foreignReactInstance' = ffi " %1[%2](%3, %4) "


aAttributesDefaults :: AAttributes
aAttributesDefaults = AAttributes Undefined Undefined Undefined

a :: (Renderable x) => Attributes -> AAttributes -> x -> DOMElement
a = ffi " require('../files/ReactWrapper').constructDOMElement(\"a\", %1, Fay$$_(%3), %2) "

span' :: (Renderable a) => Attributes -> Automatic a -> DOMElement
span' = ffi " require('../files/ReactWrapper').constructDOMElement(\"span\", %1, %2) "

span :: (Renderable x) => x -> DOMElement
span = span' defaultAttributes

div :: (Renderable x) => Attributes -> x -> DOMElement
div = ffi " require('../files/ReactWrapper').constructDOMElement(\"span\", %1, Fay$$_(%2)) "

data SyntheticEvent

eventValue :: SyntheticEvent -> Fay String
eventValue = ffi " %1['target']['value'] "

data ReactClass
data ReactInstance a
data SyntheticMouseEvent
data DOMElement

class Renderable a

instance Renderable Text
instance Renderable DOMElement

constructDOMElement :: (Renderable a) => String -> Attributes -> a -> DOMElement
constructDOMElement = ffi " require('../files/ReactWrapper').constructDOMElement(%1, %2, Fay$$_(%3)) "

constructDOMElementArray :: String -> Attributes -> [DOMElement] -> DOMElement
constructDOMElementArray = ffi " require('../files/ReactWrapper').constructDOMElement(%*) "

data ReactData a = ReactData {
  render :: ReactInstance a -> Fay DOMElement
  , componentWillMount :: ReactInstance a -> Fay()
  , componentDidMount :: ReactInstance a -> Fay()
  , componentWillUnmount :: ReactInstance a -> Fay()
  , displayName :: String
  , getInitialState :: a
}

defaultReactData :: a -> ReactData a
defaultReactData initialState = ReactData {
  render = const $ return $ constructDOMElement "div" defaultAttributes (pack "")
  , componentWillMount = const $ return ()
  , componentDidMount = const $ return ()
  , componentWillUnmount = const $ return ()
  , displayName = "<HaskellReactClass>"
  , getInitialState = initialState
}

data Attributes = Attributes {
  className :: Defined String
  , onClick :: Defined ( SyntheticMouseEvent -> Fay() )
  , id :: Defined String
}

defaultAttributes :: Attributes
defaultAttributes = Attributes {
  className = Undefined
  , onClick = Undefined
  , id = Undefined
}

declareReactClass :: ReactData a -> ReactClass
declareReactClass = ffi " require('../files/ReactWrapper').declareReactClass(%1) "

declareAndRun :: ReactData a -> DOMElement
declareAndRun = classInstance . declareReactClass

setState :: ReactInstance a -> a -> Fay()
setState = ffi " %1['setState'](Fay$$_(%2)) "

state :: ReactInstance a -> Fay a
state = ffi " %1['state'] "

isMounted :: ReactInstance a -> Fay Bool
isMounted = ffi " %1['isMounted']() "

classInstance :: ReactClass -> DOMElement
classInstance = ffi " %1(null) "

placeElement :: DOMElement -> Fay ()
placeElement = ffi " require('../files/ReactWrapper').renderReact(%1) "

getType :: SyntheticMouseEvent -> String
getType = ffi " %1['type'] "
