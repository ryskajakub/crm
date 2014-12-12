{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports #-}

module HaskellReact.ReadFay (
  ReadFay ()
  , RF (RF)
  , rf
  , readFayReturn
  , readFayBind
  , readFayThen
  , runReadFay
  , isMounted
  , state
  , props
) where

import qualified "fay-base" Prelude as P
import "fay-base" Prelude hiding((>>=), (>>), return)
import FFI (ffi, Automatic)
import HaskellReact.ComponentData (ReactThis)

newtype ReadFay a = ReadFay { runReadFay :: Fay a }

data RF = RF { 
  return :: forall a. a -> ReadFay a
  , bind :: forall a b. ReadFay a -> (a -> ReadFay b) -> ReadFay b
  , then' :: forall a b. ReadFay a -> ReadFay b -> ReadFay b
}

readFayReturn :: a -> ReadFay a
readFayReturn a = ReadFay $ P.return a

readFayBind :: ReadFay a -> (a -> ReadFay b) -> ReadFay b
readFayBind a b = ReadFay $ runReadFay a P.>>= (runReadFay . b)

readFayThen :: ReadFay a -> ReadFay b -> ReadFay b
readFayThen a b = ReadFay $ runReadFay a P.>> runReadFay b

rf :: RF
rf = RF {
  return = readFayReturn
  , bind = readFayBind
  , then' = readFayThen
}

isMounted' :: ReactThis a b -> Fay Bool
isMounted' = ffi " %1['isMounted']() "

isMounted :: ReactThis a b -> ReadFay Bool
isMounted = ReadFay . isMounted'

state' :: ReactThis a b -> Fay (Automatic a)
state' = ffi " %1['state'] "

state :: ReactThis a b -> ReadFay a
state = ReadFay . state'

props' :: ReactThis a b -> Fay (Automatic b)
props' = ffi " %1['props'] "

props :: ReactThis a b -> ReadFay b
props = ReadFay . props'

{-
Example usage of the ReadFay in the do notation

{-# LANGUAGE RebindableSyntax #-}

doNotation :: ReadFay ()
doNotation = let RF return (>>=) (>>) = rf in do
  b <- return ()
  x <- return ()
  return ()
  return ()
-}
