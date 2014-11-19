{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RankNTypes #-}

module HaskellReact.ReadFay (
  ReadFay
  , RF
  , rf
) where

import Prelude

newtype ReadFay a = ReadFay { runReadFay :: Fay a }

data RF = RF { 
  readFayReturn :: forall a. a -> ReadFay a
  , readFayBind :: forall a b. ReadFay a -> (a -> ReadFay b) -> ReadFay b
  , readFayThen :: forall a b. ReadFay a -> ReadFay b -> ReadFay b
}

rf :: RF
rf = RF {
  readFayReturn = \x -> ReadFay $ return x
  , readFayBind = \a b -> ReadFay $ runReadFay a >>= (runReadFay . b)
  , readFayThen = \a b -> ReadFay $ runReadFay a >> runReadFay b
}

-- | Example usage fo the ReadFay in the do notation
doNotation :: ReadFay ()
doNotation = let RF return (>>=) (>>) = rf in do
  b <- return ()
  x <- return ()
  return ()
  return ()
