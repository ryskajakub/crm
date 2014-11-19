{-# LANGUAGE PackageImports #-}

module Main where

import FFI
import "fay-base" Data.Text (Text, pack)
import "fay-base" Data.Var (newVar, set, subscribeChangeAndRead)
import HaskellReact
import HaskellReact.Tag.Input (input, defaultInputAttributes, onChange)
import HaskellReact.Tag.Anchor (a, defaultAnchorAttributes, AnchorAttributes(href))
import Prelude hiding (span, div, elem)

data InnerData = InnerData {
  header :: Text
}

main :: Fay ()
main = runInReact list

runInReact :: DOMElement -> Fay ()
runInReact element = placeElement $ declareAndRun $ (defaultReactData (InnerData $ pack "ahoj")) {
    render = const $ return element
  }

list :: DOMElement
list = constructDOMElement "div" defaultAttributes (Empty {}) [
  a (defaultAnchorAttributes { href = Defined $ pack "http://seznam.cz/" }) $ pack "Link"
  , span $ pack "elem 1"
  , span $ pack "elem 2"
  , textElement " elem 3"
  , phantom bootstrap
  ]

some :: Fay ()
some = let
  inst = declareAndRun $ (defaultReactData (InnerData $ pack "ahoj")) {
    render = const $
      let spanElement = span $ pack "text"
      in return $ spanElement
    }
  in placeElement inst

data ButtonData = ButtonData {
  bsStyle :: Defined String
  , title :: Defined String
}

data Empty = Empty {}

primary :: ButtonData
primary = ButtonData { bsStyle = Defined "primary", title = Defined "Buttonek" }

data ReactBootstrap
instance CommonJSModule ReactBootstrap

requireReactBootstrap :: ReactBootstrap
requireReactBootstrap = ffi " require(\"react-bootstrap\") "

-- | Creates an instance of a React Bootstrap class
reactBootstrap :: (Renderable b)
               => String -- ^ The name of the Bootstrap class
               -> Automatic a -- The props passed to the instance
               -> Automatic b -- The children passed to the instance
               -> ReactInstance
reactBootstrap = foreignReact requireReactBootstrap

bootstrap :: ReactInstance
bootstrap = reactBootstrap "DropdownButton" primary [
  phantom $ reactBootstrap "MenuItem" Empty (pack "Action") :: DOMElement
  , phantom $ reactBootstrap "MenuItem" Empty (pack "Action 2") :: DOMElement
  ]

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
      return $ constructDOMElement "div" defaultAttributes defaultAttributes [inputElement, spanElement]
    , componentDidMount = \reactInstance -> do
      putStrLn ("component mounted")
      subscribeChangeAndRead var (\v -> setState reactInstance (InnerData $ v))
      return ()
  }
  placeElement inst
