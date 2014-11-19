{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports #-}

module HaskellReact (
  module HaskellReact.Tag.Construct
  , module HaskellReact.Tag.Simple
  , module HaskellReact.Event
  , foreignReact , foreignReact'
  , AAttributes(..)
  , aAttributesDefaults
  , CommonJSModule
  , a
  , textElement
  , ReactClass, ReactThis, ReactInstance
  , ReactData(..)
  , defaultReactData
  , declareReactClass
  , declareAndRun
  , setState
  , state
  , isMounted
  , classInstance
  , placeElement
  , getType
  , phantom
) where 

import FFI
import "fay-base" Data.Text (Text, pack)
import Prelude hiding (id, span, div)
import HaskellReact.Event
import HaskellReact.Tag.Construct
import HaskellReact.Tag.Simple

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
  , getInitialState :: () -> a
}

defaultReactData :: a -> ReactData a
defaultReactData initialState = ReactData {
  render = const $ return $ constructDOMElement "div" defaultAttributes defaultAttributes (pack "")
  , componentWillMount = const $ return ()
  , componentDidMount = const $ return ()
  , componentWillUnmount = const $ return ()
  , displayName = "<HaskellReactClass>"
  , getInitialState = const $ initialState
}

declareReactClass :: ReactData a -> ReactClass
declareReactClass = ffi " require('../files/ReactWrapper').declareReactClass(%1) "

declareAndRun :: ReactData a -> ReactInstance
declareAndRun = classInstance . declareReactClass

setState :: ReactThis a -> Automatic a -> Fay ()
setState = ffi " %1['setState'](%2) "

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
