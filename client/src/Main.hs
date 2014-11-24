{-# LANGUAGE PackageImports #-}

module Main where

import HaskellReact
import "fay-base" Data.Text (pack)
import Prelude hiding (span, div, elem)
import HaskellReact.Router
import Crm.Component.Navigation (navigation)
import FFI (Defined(Defined, Undefined))

main :: Fay ()
main = router

router :: Fay ()
router = runRouter $
  route (RouteData (Defined "/") navigation Undefined) [
    route (RouteData (Defined "users") (declareInReact $ div $ pack "users") (Defined $ pack "users")) 
      ([] :: [DOMElement])
    , defaultRoute $ defaultRouteProps 
        (Defined $ pack "default") 
        (declareInReact $ div $ pack "default")
  ]

declareInReact :: DOMElement -> ReactClass a
declareInReact element = declareReactClass $ (defaultReactData
  (pack "DeclareInReact")
  (Empty {})
  (const $ readFayReturn element))
