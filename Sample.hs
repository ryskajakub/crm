{-# LANGUAGE EmptyDataDecls #-}
module Hello where

import FFI

data ReactClass
data ReactInstance
data Attributes = Attributes { className :: String }

declareReactClass :: String -> ReactClass
declareReactClass = ffi " declareReactClass(%1) "

classInstance :: ReactClass -> Attributes -> ReactInstance
classInstance = ffi " %1(%2) "

placeInstance :: ReactInstance -> Fay ()
placeInstance = ffi " renderReact(%1) "

main :: Fay ()
main = do
  let clazz = declareReactClass "h4"
  let inst = classInstance clazz (Attributes "blue")
  placeInstance inst
