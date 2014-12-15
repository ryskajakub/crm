{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module NoMain where

import "fay-base" Prelude hiding (span, div, elem)
import Data.Nullable (fromNullable)
import Data.Var (Var, newVar, subscribeAndRead, get, modify)
import FFI (ffi, Nullable)
import "fay-base" Data.Text (Text, pack)
import "fay-base" Data.Maybe (isJust)

import HaskellReact.BackboneRouter (startRouter)
import Crm.Server (fetchCompanies, fetchMachines)
import qualified Crm.Component.Navigation as Navigation
import Crm.Component.Data
import Crm.Component.Company (companiesList, companyDetail, companyNew)
import Crm.Component.Machine (machineNew)
import qualified Crm.Shared.Machine as M
import qualified Crm.Shared.Company as C

main' :: Fay ()
main' = do
  appVar' <- appVar
  router' <- startRouter [(
      pack "", const $ modify appVar' (\appState -> appState { navigation = FrontPage })
    ), (
      pack "companies/:id", \params -> let
        cId = head params
        in case (parseSafely cId, cId) of
          (Just(cId''), _) -> do
            appState <- get appVar'
            let
              companies' = companies appState
              company'' = lookup cId'' companies'
              machinesInCompany = filter ((==)cId'' . M.companyId . snd) (machines appState)
              machinesNoIds = map snd machinesInCompany
            maybe (return ()) (\company' ->
              modify appVar' (\appState' ->
                appState' {
                  navigation = CompanyDetail cId'' company' False machinesNoIds
                }
              )
              ) company''
          (_, new) | new == (pack "new") -> modify appVar' (\appState ->
            appState {
              navigation = CompanyNew C.newCompany }
            )
          _ -> return ()
    ), (
      pack "companies/:id/new-machine", \params -> do
        appState <- get appVar'
        let
          companies' = companies appState
          newAppState = case (parseSafely $ head params) of
            Just(companyId') | isJust $ lookup companyId' companies' -> MachineNew companyId'
            _ -> NotFound
        modify appVar' (\appState' -> appState' { navigation = newAppState })
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
  _ <- subscribeAndRead appVar' (\appState -> let
    frontPage = Navigation.navigation myData (companiesList myData (companies appState))
    in case navigation appState of
      FrontPage -> frontPage
      NotFound -> frontPage
      CompanyDetail companyId' company' editing' machines' ->
        Navigation.navigation myData
          (companyDetail editing' myData appVar' (companyId', company') machines')
      CompanyNew company' -> Navigation.navigation myData (companyNew myData appVar' company')
      MachineNew companyId' -> Navigation.navigation myData (machineNew myData appVar' companyId'))
  return ()

parseInt :: Text -> Nullable Int
parseInt = ffi " (function() { var int = parseInt(%1); ret = ((typeof int) === 'number' && !isNaN(int)) ? int : null; return ret; })() "

parseSafely :: Text -> Maybe Int
parseSafely possibleNumber = fromNullable $ parseInt possibleNumber

appVar :: Fay (Var AppState)
appVar = newVar defaultAppState
