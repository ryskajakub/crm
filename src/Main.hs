{-# LANGUAGE PackageImports #-}

module Main where

import FFI
import "fay-base" Data.Text (Text, pack)
import HaskellReact
import Tag.Input (input, defaultInputAttributes, onChange)
import "fay-base" Data.Var (newVar, set, subscribeChangeAndRead)
import Prelude hiding (span)

data InnerData = InnerData {
  header :: Text
}

main :: Fay ()
main = bootstrap

some :: Fay ()
some = let
  inst = declareAndRun $ (defaultReactData (InnerData $ pack "ahoj")) {
    render = const $
      let spanElement = span $ pack "text"
      in return $ spanElement
    }
  in placeElement inst

primary :: ButtonData
primary = ButtonData { bsStyle = Defined "primary" }

data ReactBootstrap
instance CommonJSModule ReactBootstrap

requireReactBootstrap :: ReactBootstrap
requireReactBootstrap = ffi " require(\"react-bootstrap\") "

-- | Creates an instance of a React Bootstrap class
reactBootstrap :: String -- ^ The name of the Bootstrap class
               -> Automatic a -- The props passed to the instance
               -> String -- The children passed to the instance
               -> DOMElement
reactBootstrap = foreignReactInstance requireReactBootstrap

bootstrap :: Fay ()
bootstrap = let
  button = reactBootstrap "Button" primary "Hit me now YAY!"
  in placeElement button

flux :: Fay ()
flux = do
  var <- newVar $ pack "Initial state"
  let inst = declareAndRun $ (defaultReactData (InnerData $ pack "ahoj")) {
    render = \reactInstance -> do
      let inputElement = input defaultAttributes (defaultInputAttributes {
        onChange = Defined $ \changeEvent -> eventValue changeEvent >>= set var . pack
      }) (pack "")
      putStrLn ("rendered")
      actualState <- state reactInstance
      let spanElement = span $ header actualState
      return $ constructDOMElementArray "div" defaultAttributes [inputElement, spanElement]
    , componentDidMount = \reactInstance -> do
      putStrLn ("component mounted")
      subscribeChangeAndRead var (\v -> setState reactInstance (InnerData $ v))
      return ()
  }
  placeElement inst
