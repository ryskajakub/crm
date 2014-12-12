{-# LANGUAGE PackageImports #-}

module NoMain where

import HaskellReact hiding (main, id)
import HaskellReact.Tag.Input
import Prelude hiding (span, div, elem)
import Data.Nullable (fromNullable)
import Data.Var (Var, newVar, subscribeAndRead, set, oneShot, get, waitFor, modify)
import FFI (ffi, Nullable, Defined(Defined))
import "fay-base" Data.Text (Text, pack, unpack, append)

import HaskellReact.BackboneRouter (startRouter, BackboneRouter, link)
import Crm.Shared.Data
import Crm.Server (fetchFromServer)

import "fay-base" Debug.Trace

main' :: Fay ()
main' = do
  routerVar' <- routerVar
  router <- startRouter [(
      pack "", const $ set routerVar' FrontPage
    ), ( 
      pack "page/:id", \id -> set routerVar' (OtherPage $ head id)
    )]
  dataVar' <- dataVar
  set dataVar' "AAAAAAa"
  userInputVar' <- userInputVar
  _ <- subscribeAndRead routerVar' (\navigationState -> let
    navigation mainElement = simpleReactBody $ div [
      div [
        link (pack "Front page") (pack "") router
        , link (pack "Other page") (pack "page/0") router
        , div $ pack "Currpage" `append` (pack $ show navigationState)
        , mainElement
      ]]
    in case navigationState of
      FrontPage -> navigation $ div $ pack "FP"
      OtherPage someId -> do
        waitFor dataVar' (\t -> t /= "") (\c -> navigation $ div $ pack c)
        modify dataVar' id
    )
  return ()

parseInt :: Text -> Nullable Int
parseInt = ffi " (function() { var int = parseInt(%1); ret = ((typeof int) === 'number' && !isNaN(int)) ? int : null; return ret; })() "

parseSafely :: Text -> Maybe Int
parseSafely possibleNumber = fromNullable $ parseInt possibleNumber

companiesVar :: Fay (Var [Company])
companiesVar = newVar []

userInputVar :: Fay (Var String)
userInputVar = newVar "AHOJ"

data NavigationState = FrontPage | OtherPage { id1 :: Text }

routerVar :: Fay (Var NavigationState)
routerVar = newVar FrontPage

dataVar :: Fay (Var String)
dataVar = newVar ""
