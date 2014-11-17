{-# LANGUAGE PackageImports #-}

module Main where

import FFI
import "fay-base" Data.Text (Text, pack)
import HaskellReact
import Tag.Input
import "fay-base" Data.Var (newVar, set, subscribeChangeAndRead)
import Prelude hiding (span)

data InnerData = InnerData {
  header :: Text
}

main :: Fay ()
main = let
  inst = declareAndRun $ (defaultReactData (InnerData $ pack "ahoj")) {
    render = const $
      let spanElement = span $ pack "text"
      in return $ spanElement
    }
  in placeElement inst

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
