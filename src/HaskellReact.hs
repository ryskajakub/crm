{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports #-}

module HaskellReact where

import FFI
import "fay-base" Data.Text (Text, pack)
import Prelude hiding (id, span)
import Event
import Tag.Construct

type URL = Text
type Rel = Text
type Target = Text

data AAttributes = AAttributes {
  href :: Defined URL
  , rel :: Defined Rel
  , target :: Defined Target
}

class CommonJSModule a

foreignReact :: (CommonJSModule b, Renderable c)
             => Automatic b -> String -> Automatic a -> Automatic c -> ReactInstance
foreignReact = ffi " %1[%2](%3, %4) "

foreignReact' :: (CommonJSModule b, Renderable c)
              => Automatic b -> String -> Automatic a -> [Automatic c] -> ReactInstance
foreignReact' = ffi " %1[%2](%3, %4) "


aAttributesDefaults :: AAttributes
aAttributesDefaults = AAttributes Undefined Undefined Undefined

a :: (Renderable x) => Attributes -> AAttributes -> x -> DOMElement
a = ffi " require('../files/ReactWrapper').constructDOMElement(\"a\", %1, Fay$$_(%3), %2) "

span' :: (Renderable a) => Attributes -> Automatic a -> DOMElement
span' = ffi " require('../files/ReactWrapper').constructDOMElement(\"span\", %1, %2) "

span :: (Renderable x) => x -> DOMElement
span = span' defaultAttributes

div' :: (Renderable a) => Attributes -> [Automatic a] -> DOMElement
div' = ffi " require('../files/ReactWrapper').constructDOMElement(\"div\", %1, %2) "

div :: (Renderable a) => [Automatic a] -> DOMElement
div = div' defaultAttributes

li' :: (Renderable x) => Attributes -> Automatic x -> DOMElement
li' = ffi " require('../files/ReactWrapper').constructDOMElement(\"li\", %1, %2) "

li :: (Renderable x) => Automatic x -> DOMElement
li = li' defaultAttributes

ul' :: (Renderable x) => Attributes -> [Automatic x] -> DOMElement
ul' = ffi " require('../files/ReactWrapper').constructDOMElement(\"ul\", %1, %2) "

ul :: (Renderable x) => [Automatic x] -> DOMElement
ul = ul' defaultAttributes

phantom :: a -> b
phantom = ffi " %1 "

textElement :: String -> DOMElement
textElement = phantom . pack

data ReactClass
data ReactThis a
data ReactInstance

data ReactData a = ReactData {
  render :: ReactThis a -> Fay DOMElement
  , componentWillMount :: ReactThis a -> Fay()
  , componentDidMount :: ReactThis a -> Fay()
  , componentWillUnmount :: ReactThis a -> Fay()
  , displayName :: String
  , getInitialState :: a
}

defaultReactData :: a -> ReactData a
defaultReactData initialState = ReactData {
  render = const $ return $ constructDOMElement "div" defaultAttributes defaultAttributes (pack "")
  , componentWillMount = const $ return ()
  , componentDidMount = const $ return ()
  , componentWillUnmount = const $ return ()
  , displayName = "<HaskellReactClass>"
  , getInitialState = initialState
}

declareReactClass :: ReactData a -> ReactClass
declareReactClass = ffi " require('../files/ReactWrapper').declareReactClass(%1) "

declareAndRun :: ReactData a -> ReactInstance
declareAndRun = classInstance . declareReactClass

setState :: ReactThis a -> a -> Fay ()
setState = ffi " %1['setState'](Fay$$_(%2)) "

state :: ReactThis a -> Fay a
state = ffi " %1['state'] "

isMounted :: ReactThis a -> Fay Bool
isMounted = ffi " %1['isMounted']() "

classInstance :: ReactClass -> ReactInstance
classInstance = ffi " %1(null) "

placeElement :: ReactInstance -> Fay ()
placeElement = ffi " require('../files/ReactWrapper').renderReact(%1) "

getType :: SyntheticMouseEvent -> String
getType = ffi " %1['type'] "
