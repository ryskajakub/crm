{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RebindableSyntax #-}

module Main where

import FFI
import "fay-base" Data.Text (Text, pack)
import HaskellReact.Tag.Hyperlink (a, defaultHyperlinkAttributes, HyperlinkAttributes(href, target), blank)
import Prelude hiding (span, div, elem)
import qualified Prelude as P
import HaskellReact.ReadFay (RF(RF), rf, readFayReturn, ReadFay, runReadFay, isMounted)
import HaskellReact

data InnerData = InnerData {
  header :: Text
}

data RouterData = RouterData {
  location :: String
}

data RouteData a = RouteData {
  path :: String
  , handler :: ReactClass a
}

router = let 
  slashClass = declareReactClass
  in reactRouter "Routes" (RouterData "history") [
    reactRouter "Route" (RouteData "/" $ declareInReact list) ([] :: [DOMElement])
  ]

main :: Fay ()
main = runInReact list

data ReactState = ReactState {
  header1 :: Text
  , countClicks :: Int
}

declareInReact :: DOMElement -> ReactClass a
declareInReact element = declareReactClass $ (defaultReactData 
  (InnerData $ pack "ahoj")
  (const $ readFayReturn element))

runInReact :: DOMElement -> Fay ()
runInReact = placeElement . classInstance . declareInReact

list :: DOMElement
list = constructDOMElement "div" defaultAttributes (Empty {}) [
  a (defaultHyperlinkAttributes { href = Defined $ pack "http://seznam.cz/", target = Defined blank }) $ pack "Link"
  , span $ pack "elem 1"
  , span $ pack "elem 2"
  , textElement " elem 3"
  , span $ pack "elem 4"
  , phantom bootstrap
  ]

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
               -> Automatic a -- ^ The props passed to the instance
               -> Automatic b -- ^ The children passed to the instance
               -> ReactInstance
reactBootstrap = foreignReact requireReactBootstrap

data ReactRouter
instance CommonJSModule ReactRouter

requireReactRouter :: ReactRouter
requireReactRouter = ffi " require(\"react-router\") "

reactRouter :: Renderable b
            => String -- ^ The name of the ReactRouter class
            -> Automatic a -- ^ Props that will be passed to the instance
            -> Automatic b -- ^ The children passed to the instance
            -> ReactInstance
reactRouter = foreignReact requireReactRouter

bootstrap :: ReactInstance
bootstrap = reactBootstrap "DropdownButton" primary [
  phantom $ reactBootstrap "MenuItem" Empty (pack "Action") :: DOMElement
  , phantom $ reactBootstrap "MenuItem" Empty (pack "Action 2") :: DOMElement
  ]
