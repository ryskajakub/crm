{-# LANGUAGE PackageImports #-}

module Main where

import HaskellReact
import "fay-base" Data.Text (Text, pack)
import Prelude hiding (span, div, elem)
import Data.Nullable (fromNullable)
import FFI (ffi, Nullable)

import HaskellReact.BackboneRouter (startRouter, BackboneRouter)
import Crm.Component.CompaniesList (companiesList)
import Crm.Component.Navigation
import Crm.Component.Data
import Crm.Server (fetchFromServer)

import Debug.Trace

mainStartState :: MainState
mainStartState = MainState {
  routerState = CompaniesList
  , router = Nothing
}

modifyMainState :: RouterState -> MainState -> MainState
modifyMainState routerState' mainState = mainState {
  routerState = routerState'
}

main :: Fay ()
main = do
  fetchFromServer
  placeElementToBody $ classInstance $ declareReactClass $
    (reactData (pack "CrmRouter") (mainStartState) (\reactThis ->
      state reactThis `readFayBind` \mainState -> let
        router' = router mainState
        in case routerState mainState of
          CompaniesList -> companiesList router'
    )) {
      componentWillMount = \reactThis -> do
        router' <- startRouter [(pack "", const $ do
            state' <- (runReadFay $ state reactThis)
            let newState = modifyMainState CompaniesList state'
            setState reactThis newState
          )]
        state' <- runReadFay $ state reactThis
        setState reactThis $ state' {
          router = Just router'
        }
    }

parseInt :: Text -> Nullable Int
parseInt = ffi " (function() { var int = parseInt(%1); ret = ((typeof int) === 'number' && !isNaN(int)) ? int : null; return ret; })() "

parseSafely :: Text -> Maybe Int
parseSafely possibleNumber = fromNullable $ parseInt possibleNumber
