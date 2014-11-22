module HaskellReact.Bootstrap where

import HaskellReact (foreignReact, Renderable, CommonJSModule, ReactInstance)
import FFI (ffi, Automatic, Defined)

data ReactBootstrap
instance CommonJSModule ReactBootstrap

data ButtonData = ButtonData {
  bsStyle :: Defined String
  , title :: Defined String
}

requireReactBootstrap :: ReactBootstrap
requireReactBootstrap = ffi " require(\"react-bootstrap\") "

-- | Creates an instance of a React Bootstrap class
reactBootstrap :: (Renderable b)
               => String -- ^ The name of the Bootstrap class
               -> Automatic a -- ^ The props passed to the instance
               -> Automatic b -- ^ The children passed to the instance
               -> ReactInstance
reactBootstrap = foreignReact requireReactBootstrap
