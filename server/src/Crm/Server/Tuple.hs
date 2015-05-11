{-# LANGUAGE TemplateHaskell #-}

module Crm.Server.Tuple where

import Language.Haskell.TH
import Control.Monad (forM)

-- | generates function modifying single element in a tuple
modT :: Int -- ^ length of the input tuple
     -> Int -- ^ index of the tuple starting with 0
     -> Q Exp -- ^ (b -> c) -> (a1,a2,b,a3,a4) -> (a1,a2,c,a3,a4)
modT length element = do
  mapFunction <- newName "x"
  names <- forM [1..length] $ const $ newName "x"
  let 
    parameters = map VarP names
    results = map VarE names
    (start, x:xs) = splitAt element results
    finalResults = start ++ [AppE (VarE mapFunction) x] ++ xs
  return $ LamE [VarP mapFunction] ( LamE [ TupP parameters ] $ TupE finalResults )
