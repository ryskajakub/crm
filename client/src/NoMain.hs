{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module NoMain where

import HaskellReact hiding (main, id)
import HaskellReact.Tag.Input
import "fay-base" Prelude hiding (span, div, elem)
import Data.Nullable (fromNullable)
import Data.Var (Var, newVar, subscribeAndRead, set, oneShot, get, waitFor, modify, withUnsubscriber, newRef)
import "fay-base" Data.Maybe (whenJust, isJust)
import FFI (ffi, Nullable, Defined(Defined))
import "fay-base" Data.Text (Text, pack, unpack, append)

import HaskellReact.BackboneRouter (startRouter, BackboneRouter, link)
import qualified Crm.Shared.Company as C
import qualified Crm.Shared.Machine as M
import Crm.Server (fetchCompanies, fetchMachines)
import qualified Crm.Component.Navigation as Navigation
import Crm.Component.Data
import Crm.Component.Company (companiesList, companyDetail)

import "fay-base" Debug.Trace

main' :: Fay ()
main' = do
  appVar' <- appVar
  router' <- startRouter [(
      pack "", const $ modify appVar' (\appState -> appState { navigation = FrontPage })
    ), (
      pack "companies/:id", \cId ->
        let cId' = parseSafely (head cId)
        in whenJust cId' (\cId'' -> do
          appState <- get appVar'
          let
            companies' = companies appState
            company'' = lookup cId'' companies'
            machines' = map snd (machines appState)
          maybe (return ()) (\company' ->
            modify appVar' (\appState' ->
              appState' {
                navigation = CompanyDetail cId'' company' False machines'
              }
            )
            ) company''
          )
    )]
  let myData = MyData router'
  fetchCompanies (\companies' ->
    modify appVar' (\appState ->
      appState { companies = companies' }
    ))
  fetchMachines (\machines' ->
    modify appVar' (\appState ->
      appState { machines = machines' }
    ))
  _ <- subscribeAndRead appVar' (\appState ->
    case navigation appState of
      FrontPage -> Navigation.navigation myData (companiesList myData (companies appState))
      CompanyDetail companyId' company' editing' machines' ->
        Navigation.navigation myData
          (companyDetail editing' myData appVar' (companyId', company') machines'))
  return ()

parseInt :: Text -> Nullable Int
parseInt = ffi " (function() { var int = parseInt(%1); ret = ((typeof int) === 'number' && !isNaN(int)) ? int : null; return ret; })() "

parseSafely :: Text -> Maybe Int
parseSafely possibleNumber = fromNullable $ parseInt possibleNumber

appVar :: Fay (Var AppState)
appVar = newVar defaultAppState
