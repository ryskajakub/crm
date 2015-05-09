{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Playground where

import HaskellReact
import FFI
import "fay-base" Data.Text (Text, fromString, showInt, (<>))
import "fay-base" Prelude hiding (div)
import "fay-base" Data.Var (Var, set, subscribeAndRead, newVar)

import qualified HaskellReact.Tag.Input as I


data State = State Int


developedElement :: State -> (State -> Fay()) -> DOMElement
developedElement (State state) setState = let
  inputAttrs = I.mkInputAttrs {
    I.defaultValue = Defined $ showInt (state + 10) ,
    I.value_ = if state == 10 then Undefined else Defined $ showInt state }
  input = I.input (click $ setState $ State $ state + 1) inputAttrs
  in input


renderLoop :: Fay ()
renderLoop = do
  appState' <- newVar $ State 10
  _ <- subscribeAndRead appState' $ \appState -> let
    mod state = set appState' state
    element1 = developedElement appState mod
    in simpleReactBody' element1 (return ())
  return ()
