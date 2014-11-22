{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RebindableSyntax #-}

module Main where

import FFI
import "fay-base" Data.Text (Text, pack)
import HaskellReact.Tag.Hyperlink (a, defaultHyperlinkAttributes, HyperlinkAttributes(href, target), blank)
import Prelude hiding (span, div, elem)
import qualified Prelude as P
import HaskellReact.ReadFay (readFayReturn)
import HaskellReact.Router
import HaskellReact.Bootstrap
import HaskellReact

data InnerData = InnerData {
  header :: Text
}

router :: ReactInstance
router = let 
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

data Empty = Empty {}

primary :: ButtonData
primary = ButtonData { bsStyle = Defined "primary", title = Defined "Buttonek" }

bootstrap :: ReactInstance
bootstrap = reactBootstrap "DropdownButton" primary [
  phantom $ reactBootstrap "MenuItem" Empty (pack "Action") :: DOMElement
  , phantom $ reactBootstrap "MenuItem" Empty (pack "Action 2") :: DOMElement
  ]
