{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Playground where

import HaskellReact
import FFI (ffi)
import "fay-base" Data.Text (Text, fromString, showInt, (<>))
import "fay-base" Prelude hiding (div)
import "fay-base" Data.Var (Var, set, subscribeAndRead, newVar)

import qualified HaskellReact.Tag.Input as I

data State = State Int

developedElement :: State -> (State -> Fay()) -> (DOMElement, Fay ())
developedElement (State state) setState = (div' (click $ setState $ State $ state + 1) ("ahoj " <> showInt state), return ())
  
renderLoop :: Fay ()
renderLoop = do
  appState' <- newVar $ State 0
  _ <- subscribeAndRead appState' $ \appState -> let
    mod state = set appState' state
    (element1, callback) = developedElement appState mod
    in simpleReactBody' element1 callback
  return ()
