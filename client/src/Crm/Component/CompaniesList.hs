{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Crm.Component.CompaniesList (
  companiesList
) where

import HaskellReact
import Crm.Component.Navigation (navigation)
import Crm.Shared.Data
import "fay-base" Data.Text (fromString, Text, unpack, pack, append, showInt)
import Prelude hiding (div, span, id)
import Data.Var (Var, subscribeAndRead)
import Data.Maybe (fromMaybe, whenJust, fromJust)
import Data.Defined (fromDefined)
import FFI (Defined(Defined, Undefined))
import HaskellReact.BackboneRouter (BackboneRouter, link)
import qualified HaskellReact.Bootstrap as B
import qualified HaskellReact.Bootstrap.Glyphicon as G

data CompaniesListState = CompaniesListState {
  companies :: Defined [Company]
  , unsubscribe :: Defined (() -> Fay())
}

startingCompaniesState :: CompaniesListState
startingCompaniesState = CompaniesListState {
  companies = Undefined
  , unsubscribe = Undefined
}

companiesListBody :: Var [Company]
                  -> Maybe BackboneRouter
                  -> ReactClass a
companiesListBody companiesVar router = let
  router' = fromJust router -- TODO unsafe
  data' = (reactData "CompaniesListBody" startingCompaniesState (\reactThis ->
    state reactThis `readFayBind` \companiesListState ->
      readFayReturn $ tbody $ map (\company ->
      tr [
        td $ link (pack $ companyName company) ("/companies/0") router
        , td $ pack $ plant company
      ]) (fromMaybe [] $ fromDefined $ companies companiesListState)
    )) {
      componentDidMount = \reactThis -> do
        unsubscribe' <- subscribeAndRead companiesVar (\newCompanies -> 
          setState reactThis (startingCompaniesState { companies = Defined newCompanies })
          )
        setState reactThis (startingCompaniesState { unsubscribe = Defined unsubscribe' })
      , componentWillUnmount = \reactThis -> do
        state' <- runReadFay $ state reactThis
        whenJust (fromDefined $ unsubscribe state') (\doUnsubscribe -> doUnsubscribe ())
    }
  in declareReactClass data'

companiesList :: Maybe BackboneRouter -- ^ Router from which the link can be created
              -> (Var [Company])
              -> ReadFay DOMElement
companiesList router companiesVar = readFayReturn $ let
  element = main [
    section $
      B.button [
        G.plus
        , text2DOM "Přidat firmu"
      ]
    , section $
      B.table [
        thead $ tr [
          th "Název firmy"
          , th "Platnost servisu vyprší za"
        ]
        , reactInstance2DOM $ classInstance $ companiesListBody companiesVar router
      ]
    ]
  elementWithNavigation = reactInstance2DOM $ classInstance $ navigation router element
  in elementWithNavigation
