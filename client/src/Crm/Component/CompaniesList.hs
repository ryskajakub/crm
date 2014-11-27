{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Crm.Component.CompaniesList (
  companiesList
) where

import HaskellReact
import Crm.Component.Navigation (navigation)
import Crm.Component.Data
import "fay-base" Data.Text (fromString, Text, unpack)
import Prelude hiding (div, span)
import Data.Var (Var, subscribeAndRead)
import Data.Maybe (fromMaybe)
import Data.Defined (fromDefined)
import FFI (Defined(Defined, Undefined))
import HaskellReact.BackboneRouter (BackboneRouter)
import qualified HaskellReact.Bootstrap as B
import qualified HaskellReact.Bootstrap.Glyphicon as G

import Debug.Trace

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
                  -> ReactClass a
companiesListBody companiesVar = let
  data' = (reactData "CompaniesListBody" startingCompaniesState (\reactThis ->
    state reactThis `readFayBind` \companiesListState ->
      readFayReturn $ tbody $ map (\company ->
      tr [
        td $ name company
        , td $ plant company
      ]) (fromMaybe [] $ fromDefined $ companies companiesListState)
    )) {
      componentDidMount = \reactThis -> do
        unsubscribe' <- subscribeAndRead companiesVar (\newCompanies -> 
          setState reactThis (startingCompaniesState { companies = Defined newCompanies })
          )
        setState reactThis (startingCompaniesState { unsubscribe = Defined unsubscribe' })
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
        , reactInstance2DOM $ classInstance $ companiesListBody companiesVar
      ]
    ]
  elementWithNavigation = reactInstance2DOM $ classInstance $ navigation router element
  in elementWithNavigation
