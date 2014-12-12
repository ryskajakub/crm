{-# LANGUAGE PackageImports #-}

module NoMain where

import HaskellReact hiding (main, id)
import HaskellReact.Tag.Input
import Prelude hiding (span, div, elem)
import Data.Nullable (fromNullable)
import Data.Var (Var, newVar, subscribeAndRead, set, oneShot, get, waitFor, modify, withUnsubscriber, newRef)
import FFI (ffi, Nullable, Defined(Defined))
import "fay-base" Data.Text (Text, pack, unpack, append)

import HaskellReact.BackboneRouter (startRouter, BackboneRouter, link)
import Crm.Shared.Data
import Crm.Server (fetchFromServer)
import Crm.Component.Navigation (navigation)
import Crm.Component.Data (MyData(MyData))
import Crm.Component.CompaniesList (companiesList)

import "fay-base" Debug.Trace

main' :: Fay ()
main' = do
  routerVar' <- routerVar
  router <- startRouter [(
      pack "", const $ set routerVar' FrontPage
    ), (
      pack "page/:id", \id -> set routerVar' (OtherPage $ head id)
    )]
  let myData = MyData router
  companiesVar' <- companiesVar
  fetchFromServer companiesVar'
  _ <- subscribeAndRead routerVar' (\navigationState -> 
    case navigationState of
      FrontPage -> myWaitFor' companiesVar' (\companies ->
        navigation myData (companiesList myData companies)
        )
      OtherPage someId -> undefined
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

userInputVar :: Fay (Var String)
userInputVar = newVar "AHOJ"

data NavigationState = FrontPage | OtherPage { id1 :: Text }

routerVar :: Fay (Var NavigationState)
routerVar = newVar FrontPage

dataVar :: Fay (Var String)
dataVar = newVar ""
