{-# LANGUAGE PackageImports #-}

module HaskellReact.Component (
  --ReactData (componentWillMount, componentDidMount, componentWillUnmount, displayName)
  ReactData (..)
  , ReactClass
  , ReactThis
  , ReactInstance
  , defaultReactData
  , declareReactClass
  , declareAndRun
  , setState
  , classInstance
  , placeElement
  , CommonJSModule
  , foreignReact 
) where

import FFI (ffi, Automatic)
import HaskellReact.Tag.Construct
import HaskellReact.ReadFay (ReadFay, runReadFay)
import HaskellReact.ComponentData (ReactThis)

data ReactClass
data ReactInstance

data ReactData a = ReactData {
  render :: ReactThis a -> Fay DOMElement -- ^ only enable applying read functions to the state instance, forbid setting the state and such
  , componentWillMount :: ReactThis a -> Fay()
  , componentDidMount :: ReactThis a -> Fay()
  , componentWillUnmount :: ReactThis a -> Fay()
  , displayName :: String
  , getInitialState :: () -> a
}

defaultReactData :: a -> (ReactThis a -> ReadFay DOMElement) -> ReactData a
defaultReactData initialState safeRender = ReactData {
  render = runReadFay . safeRender
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
