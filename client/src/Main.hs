{-# LANGUAGE PackageImports #-}

module Main where

import HaskellReact
import "fay-base" Data.Text (Text, pack)
import Prelude hiding (span, div, elem)
import FFI (ffi, Nullable)
import HaskellReact.BackboneRouter (startRouter)
import Data.Nullable (fromNullable)

data RouterState = Slash | Company Int deriving Show

main :: Fay ()
main = placeElementToBody $ classInstance $ declareReactClass $
  (reactData (pack "Yay") (Company 5) (\reactThis ->
    readFayReturn $ div $ pack $ show $ state reactThis
  )) {
    componentWillMount = (\reactThis ->
      startRouter [(pack "company/:id", \params -> let
        companyId' = parseSafely $ head params
        in case companyId' of
          Just (companyId) -> setState reactThis $ Company companyId
          Nothing -> putStrLn "Unsupported route."
        ), (pack "", \params -> do
          putStrLn "at root"
          setState reactThis Slash
        )]
    )
  }

parseInt :: Text -> Nullable Int
parseInt = ffi " (function() { var int = parseInt(%1); ret = ((typeof int) === 'number' && !isNaN(int)) ? int : null; console.log(ret); return ret; })() "

parseSafely :: Text -> Maybe Int
parseSafely possibleNumber = fromNullable $ parseInt possibleNumber
