{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP #-}

module Main where

import HaskellReact hiding (main)
import "fay-base" Prelude hiding (span, div, elem)
import "fay-base" Data.Text (Text, pack)
import "fay-base" Data.Nullable (fromNullable)
import Data.Var (Var, newVar)
import "fay-base" FFI (ffi, Nullable)

import HaskellReact.BackboneRouter (startRouter, BackboneRouter)
import Crm.Component.CompaniesList (companiesList)
import Crm.Component.Navigation
import Crm.Component.Data
import Crm.Server (fetchFromServer)

import Debug.Trace

-- hack, so the code can be type-checked by hdev-tools,
-- maybe there is an easier way like compiling the code as library or 
-- something
#ifdef FAY
type TheIO a = Fay a
fakeCoerce :: Fay () -> TheIO ()
fakeCoerce = id
#else
import qualified "base" Prelude as P
type TheIO a = P.IO a
fakeCoerce :: Fay () -> TheIO ()
fakeCoerce = undefined
#endif

mainStartState :: MainState
mainStartState = MainState {
  routerState = CompaniesList
  , router = Nothing
}

modifyMainState :: RouterState -> MainState -> MainState
modifyMainState routerState' mainState = mainState {
  routerState = routerState'
}

main :: TheIO ()
main = fakeCoerce $ do
  companiesVar' <- companiesVar
  fetchFromServer companiesVar'
  placeElementToBody $ classInstance $ declareReactClass $
    (reactData (pack "CrmRouter") (mainStartState) (\reactThis ->
      state reactThis `readFayBind` \mainState -> let
        router' = router mainState
        in case routerState mainState of
          CompaniesList -> companiesList router' companiesVar'
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

companiesVar :: Fay (Var [Company])
companiesVar = newVar [Company (pack "1") (pack "2")]
