{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module NoMain where

import HaskellReact hiding (main, id)
import HaskellReact.Tag.Input
import "fay-base" Prelude hiding (span, div, elem)
import Data.Nullable (fromNullable)
import Data.Var (Var, newVar, subscribeAndRead, set, oneShot, get, waitFor, modify, withUnsubscriber, newRef)
import "fay-base" Data.Maybe (whenJust)
import FFI (ffi, Nullable, Defined(Defined))
import "fay-base" Data.Text (Text, pack, unpack, append)

import HaskellReact.BackboneRouter (startRouter, BackboneRouter, link)
import Crm.Shared.Company as D
import qualified Crm.Shared.Machine as M
import Crm.Server (fetchCompanies, fetchMachines)
import Crm.Component.Navigation (navigation)
import Crm.Component.Data (MyData(MyData))
import Crm.Component.Company (companiesList, companyDetail)

import "fay-base" Debug.Trace

main' :: Fay ()
main' = do
  routerVar' <- routerVar
  router <- startRouter [(
      pack "", const $ set routerVar' FrontPage
    ), (
      pack "companies/:id", \cId ->
        let cId' = parseSafely (head cId)
        in whenJust cId' (\cId'' -> set routerVar' (CompanyDetail cId''))
    )]
  let myData = MyData router
  companiesVar' <- companiesVar
  machinesVar' <- machinesVar
  fetchCompanies companiesVar'
  fetchMachines machinesVar'
  _ <- subscribeAndRead routerVar' (\navigationState ->
    case navigationState of
      FrontPage -> myWaitFor' companiesVar' (\companies ->
        navigation myData (companiesList myData companies)
        )
      CompanyDetail cId -> myWaitFor' companiesVar' (\companies ->
        myWaitFor' machinesVar' (\machines ->
          let company = find (\company -> D.companyId company == cId) companies
          in whenJust company (\company' -> let
            cId = D.companyId company'
            isMachineInCompany machine = cId' == cId where
              cId' = M.companyId machine
            machinesInCompany = filter isMachineInCompany machines
            in navigation myData (companyDetail myData company' machinesInCompany))
        ))
    )
  return ()

myWaitFor' :: Var (Maybe a) -> (a -> Fay ()) -> Fay ()
myWaitFor' v f = myWaitFor v id f

myWaitFor :: Var a -> (a -> Maybe b) -> (b -> Fay ()) -> Fay ()
myWaitFor v p f = do
  unsubscriber <- newRef Nothing
  unsubscribe <- subscribeAndRead v (\elem ->
    case p(elem) of
      Just r -> do
        unsubscriber' <- get unsubscriber
        case unsubscriber' of
          Just(unsubscribe') -> unsubscribe' ()
          Nothing -> return ()
        f(r)
      Nothing ->
        return ()
    )
  set unsubscriber (Just unsubscribe)

parseInt :: Text -> Nullable Int
parseInt = ffi " (function() { var int = parseInt(%1); ret = ((typeof int) === 'number' && !isNaN(int)) ? int : null; return ret; })() "

parseSafely :: Text -> Maybe Int
parseSafely possibleNumber = fromNullable $ parseInt possibleNumber

companiesVar :: Fay (Var (Maybe [Company]))
companiesVar = newVar Nothing

machinesVar :: Fay (Var (Maybe [M.Machine]))
machinesVar = newVar Nothing

userInputVar :: Fay (Var String)
userInputVar = newVar "AHOJ"

data NavigationState = FrontPage | CompanyDetail { companyId :: Int }

routerVar :: Fay (Var NavigationState)
routerVar = newVar FrontPage

dataVar :: Fay (Var String)
dataVar = newVar ""
