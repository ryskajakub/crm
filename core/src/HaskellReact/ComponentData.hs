{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports #-}

module HaskellReact.ComponentData ( 
  Empty(Empty)
  , ReactClass
  , ReactThis
  , ReactInstance
) where

import "fay-base" Prelude

data Empty = Empty {}

data ReactThis a b
data ReactClass a
data ReactInstance
