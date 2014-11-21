{-# LANGUAGE PackageImports #-}

module HaskellReact.Component (
  ReactData (componentWillMount, componentDidMount, componentWillUnmount, displayName)
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

data ReactClass a
data ReactInstance

data ReactData a b = ReactData {
  render :: ReactThis a b -> Fay DOMElement -- ^ only enable applying read functions to the state instance, forbid setting the state and such
  , componentWillMount :: ReactThis a b -> Fay()
  , componentDidMount :: ReactThis a b -> Fay()
  , componentWillUnmount :: ReactThis a b -> Fay()
  , displayName :: String
  , getInitialState :: () -> a
}

defaultReactData :: a -> (ReactThis a b -> ReadFay DOMElement) -> ReactData a b
defaultReactData initialState safeRender = ReactData {
  render = runReadFay . safeRender
  , componentWillMount = const $ return ()
  , componentDidMount = const $ return ()
  , componentWillUnmount = const $ return ()
  , displayName = "<HaskellReactClass>"
  , getInitialState = const $ initialState
}

declareReactClass :: ReactData a b -> ReactClass b
declareReactClass = ffi " require('../files/ReactWrapper').declareReactClass(%1) "

declareAndRun :: ReactData a b -> ReactInstance
declareAndRun = classInstance . declareReactClass

setState :: ReactThis a b -> Automatic a -> Fay ()
setState = ffi " %1['setState'](%2) "

-- | Create propless react instance
classInstance :: ReactClass a -> ReactInstance
classInstance = ffi " %1(null) "

-- | Pass the props to the React Class
classInstance' :: ReactClass a -> Automatic a -> ReactInstance
classInstance' = ffi " %1(%2) "

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
