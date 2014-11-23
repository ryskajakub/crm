{-# LANGUAGE PackageImports #-}

module Main where

import HaskellReact
import "fay-base" Data.Text (pack)
import Prelude hiding (span, div, elem)
import HaskellReact.Router
import HaskellReact.ReadFay (readFayReturn)

main :: Fay ()
main = router

data Empty = Empty {}

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
  route (RouteData "/" $ navigation) (
    route (RouteData "users" $ declareInReact $ div $ pack "LLL") ([] :: [DOMElement])
  )

declareInReact :: DOMElement -> ReactClass a
declareInReact element = declareReactClass $ (defaultReactData
  (Empty {})
  (const $ readFayReturn element))
