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

data' :: ReactData Empty a
data' = defaultReactData (Empty {}) (
  const $ readFayReturn $ 
    div [
      span $ pack "AAAAAA"
      , span $ reactRouter "RouteHandler" (Empty {}) ([] :: [DOMElement])
    ]
  )

navigation :: ReactClass a
navigation = declareReactClass data'

router :: Fay ()
router = runRouter $
  reactRouter "Route" (RouteData "/" $ navigation) (
    reactRouter "Route" (RouteData "users" $ declareInReact $ div $ pack "LLL") ([] :: [DOMElement])
  )

main :: Fay ()
main = router

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
  , textElement "elem 3"
  , span $ pack "elem 4"
  , span $ pack "elem 5"
  , phantom bootstrap
  ]

data Empty = Empty {}
