{-# LANGUAGE PackageImports #-}

module HaskellReact.Component (
  ReactData (componentWillMount, componentDidMount, componentWillUnmount, displayName)
  , ReactClass
  , ReactThis
  , ReactInstance
  , reactData
  , declareReactClass
  , statelessReactData
  , declareStateless
  , declareAndRun
  , setState
  , classInstance, classInstance'
  , placeElement
  , CommonJSModule
  , foreignReact
) where

import FFI (ffi, Automatic, Ptr)
import HaskellReact.Tag.Construct
import HaskellReact.ReadFay (ReadFay, runReadFay)
import HaskellReact.ComponentData (ReactThis, ReactClass, ReactInstance)
import "fay-base" Data.Text (Text)

data ReactData a b = ReactData {
  render :: ReactThis a b -> Fay DOMElement -- ^ only enable applying read functions to the state instance, forbid setting the state and such
  , componentWillMount :: ReactThis a b -> Fay ()
  , componentDidMount :: ReactThis a b -> Fay ()
  , componentWillUnmount :: ReactThis a b -> Fay ()
  , displayName :: Text
  , getInitialState :: () -> a
}

reactData :: Text -> a -> (ReactThis a b -> ReadFay DOMElement) -> ReactData a b
reactData className initialState safeRender = ReactData {
  render = runReadFay . safeRender
  , componentWillMount = const $ return ()
  , componentDidMount = const $ return ()
  , componentWillUnmount = const $ return ()
  , displayName = className
  , getInitialState = const $ initialState
}

statelessReactData :: Text
                   -> (ReactThis () b -> ReadFay DOMElement) -- | Render function
                   -> ReactData () b -- | React class
statelessReactData className render' = reactData className () render'

declareStateless :: Text
                 -> (ReactThis () b -> ReadFay DOMElement) -- | Render function
                 -> ReactClass b
declareStateless className = declareReactClass . statelessReactData className

declareReactClass :: ReactData a b -> ReactClass b
declareReactClass = ffi "\
\ (function(data) {\
  \ var React = require('react');\
  \ return React.createClass({\
    \ render: function() {\
      \ return data.render(this);\
    \ }\
    \ , componentWillMount: function () { return data.componentWillMount(this); }\
    \ , componentDidMount: function () { return data.componentDidMount(this); }\
    \ , componentWillUnmount: function () { return data.componentWillUnmount(this); }\
    \ , displayName: data.displayName\
    \ , getInitialState: data.getInitialState\
  \ });\
\ })(%1)\
\ "

declareAndRun :: ReactData a b -> ReactInstance
declareAndRun = classInstance . declareReactClass

setState :: ReactThis a b -> Automatic a -> Fay ()
setState = ffi " %1['setState'](%2) "

-- | Create propless react instance
classInstance :: ReactClass a -> ReactInstance
classInstance = ffi " (function () { return require('react')['createElement'](%1); })() "

-- | Pass the props to the React Class
classInstance' :: ReactClass a -- | React class to instantiate
               -> Automatic a -- | Props
               -> ReactInstance
classInstance' = ffi " (function () { return require('react')['createElement'](%1, %2); })() "

placeElement :: ReactInstance -> Fay ()
placeElement = ffi "\
\ (function(component) {\
  \ var React = require('react');\
  \ React.renderComponent (\
    \ component, document.getElementById('main')\
  \ );\
\ })(%1)\
\ "

class CommonJSModule a

foreignReact :: (CommonJSModule b, Renderable c)
             => Automatic b -- ^ module imported with CommonJS's @require()@
             -> Text -- ^ name of the property in the module
             -> Automatic a -- ^ props passed to the React class
             -> Automatic c -- ^ children passed to the React class
             -> ReactInstance
foreignReact = ffi " (function () { return (require('react')['createElement'])(%1[%2], %3, %4); })() "
