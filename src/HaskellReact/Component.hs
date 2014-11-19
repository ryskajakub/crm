{-# LANGUAGE PackageImports #-}

module HaskellReact.Component where

import FFI (ffi, Automatic)
import "fay-base" Data.Text (pack)
import HaskellReact.Tag.Construct
import HaskellReact.Tag.Simple (div)
import HaskellReact.ReadFay (ReadFay, readFayReturn)
import Prelude hiding (div)

data ReactClass
data ReactThis a
data ReactInstance

data ReactData a = ReactData {
  render :: ReactThis a -> ReadFay DOMElement -- ^ only enable applying read functions to the state instance, forbid setting the state and such
  , componentWillMount :: ReactThis a -> Fay()
  , componentDidMount :: ReactThis a -> Fay()
  , componentWillUnmount :: ReactThis a -> Fay()
  , displayName :: String
  , getInitialState :: () -> a
}

defaultReactData :: a -> ReactData a
defaultReactData initialState = ReactData {
  render = const $ readFayReturn $ div $ pack ""
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

state :: ReactThis a -> ReadFay a
state = ffi " %1['state'] "

isMounted :: ReactThis a -> Fay Bool
isMounted = ffi " %1['isMounted']() "

classInstance :: ReactClass -> ReactInstance
classInstance = ffi " %1(null) "

placeElement :: ReactInstance -> Fay ()
placeElement = ffi " require('../files/ReactWrapper').renderReact(%1) "

class CommonJSModule a

foreignReact :: (CommonJSModule b, Renderable c)
             => Automatic b
             -> String
             -> Automatic a
             -> Automatic c
             -> ReactInstance
foreignReact = ffi " %1[%2](%3, %4) "
