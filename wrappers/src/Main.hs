{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import FFI
import "fay-base" Data.Text (Text, fromString)
import HaskellReact.Tag.Hyperlink (a, defaultHyperlinkAttributes, HyperlinkAttributes(href, target), blank)
import Prelude hiding (span, div, elem, fromString)
import qualified Prelude as P
import HaskellReact.ReadFay (readFayReturn)
import HaskellReact.Router
import HaskellReact.Bootstrap
import HaskellReact

data InnerData = InnerData {
  header :: Text
}

navigation :: ReactClass a
navigation = declareReactClass $ reactData "Navigation" (Empty {}) (
  const $ readFayReturn $ 
    div [
      span $ "AAAAAA"
      , span $ routeHandler
    ]
  )

router :: Fay ()
router = runRouter $
  route (routeProps "root" navigation (Defined "/")) [
    reactInstance2DOM $ route 
      (routeProps "users" (declareInReact $ div $ "LLL") (Defined "users"))
      ([] :: [DOMElement])
    , reactInstance2DOM $ route
      (routeProps "aaa" (declareInReact $ div $ "aaa") (Defined "aaa"))
      ([] :: [DOMElement])
  ]

main :: Fay ()
main = router

data ReactState = ReactState {
  header1 :: Text
  , countClicks :: Int
}

declareInReact :: DOMElement -> ReactClass a
declareInReact element = declareReactClass $ (reactData 
  "GenericElement"
  (InnerData "ahoj")
  (const $ readFayReturn element))

runInReact :: DOMElement -> Fay ()
runInReact = placeElement . classInstance . declareInReact

list :: DOMElement
list = div [
  a (defaultHyperlinkAttributes { href = Defined $ "http://seznam.cz/", target = Defined blank }) "Link"
  , span "elem 1"
  , span "elem 2"
  , span "elem 4"
  , span "elem 5"
  ]
