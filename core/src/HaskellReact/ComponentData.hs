{-# LANGUAGE EmptyDataDecls #-}

module HaskellReact.ComponentData ( 
  Empty(Empty)
  , ReactClass
  , ReactThis
  , ReactInstance
) where

data Empty = Empty {}

data ReactThis a b
data ReactClass a
data ReactInstance
