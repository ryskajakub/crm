{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Router (
  startRouter ,
  navigate ,
  link ,
  CrmRouter ,
  CrmRoute ) where

import "fay-base" Data.Text (fromString, unpack, pack, append, showInt, Text)
import "fay-base" Prelude hiding (div, span, id)
import "fay-base" FFI (Automatic)
import "fay-base" Data.Var (Var, modify, get)
import "fay-base" Data.Function (fmap)
import "fay-base" Data.Maybe (isJust)

import qualified HaskellReact.BackboneRouter as BR
import HaskellReact
import Moment (now, requireMoment, day)

import Crm.Server (fetchCompanies, fetchMachines, fetchUpkeeps, fetchMachine)
import Crm.Helpers (parseSafely)
import qualified Crm.Shared.Machine as M
import qualified Crm.Shared.UpkeepMachine as UM
import qualified Crm.Shared.Upkeep as U
import qualified Crm.Shared.Company as C
import qualified Crm.Shared.YearMonthDay as YMD
import qualified Crm.Component.Data as D

newtype CrmRouter = CrmRouter BR.BackboneRouter
newtype CrmRoute = CrmRoute Text

startRouter :: Var D.AppState -> Fay CrmRouter
startRouter appVar = fmap CrmRouter $ BR.startRouter [(
    "", const $ modify appVar (\appState -> appState { D.navigation = D.FrontPage })
  ),(
    "companies/:id", \params -> let
      cId = head params
      in case (parseSafely cId, cId) of
        (Just(cId''), _) -> do
          appState <- get appVar
          let
            companies' = D.companies appState
            company'' = lookup cId'' companies'
            machinesInCompany = filter ((==)cId'' . M.companyId . snd) (D.machines appState)
          maybe (return ()) (\company' ->
            modify appVar (\appState ->
              appState {
                D.navigation = D.CompanyDetail cId'' company' False machinesInCompany
              }
            )
            ) company''
        (_, new) | new == "new" -> modify appVar (\appState ->
          appState {
            D.navigation = D.CompanyNew C.newCompany }
          )
        _ -> return ()
  ),(
    "companies/:id/new-machine", \params -> do
      appState <- get appVar
      let
        companies' = D.companies appState
        newAppState = case (parseSafely $ head params) of
          Just(companyId') | isJust $ lookup companyId' companies' -> let
            newMachine' = M.newMachine companyId'
            in D.MachineNew newMachine' False
          _ -> D.NotFound
      modify appVar (\appState -> appState { D.navigation = newAppState })
  ),(
    "companies/:id/new-maintenance", \params -> do
      appState <- get appVar
      let
        companies' = D.companies appState
        newAppState = case (parseSafely $ head params) of
          Just(companyId') | isJust $ lookup companyId' companies' -> let
            machines' = filter (\(_,machine') -> M.companyId machine' == companyId') (D.machines appState)
            notCheckedUpkeepMachines = map (\(id',_) -> UM.newUpkeepMachine id') machines'
            (nowYear, nowMonth, nowDay) = day $ now requireMoment
            nowYMD = YMD.YearMonthDay nowYear nowMonth nowDay YMD.DayPrecision
            in D.UpkeepNew (U.newUpkeep nowYMD) machines' notCheckedUpkeepMachines False companyId'
          _ -> D.NotFound
      modify appVar (\appState -> appState { D.navigation = newAppState })
  ),(
    "companies/:id/maintenances", \params -> do
      appState <- get appVar
      let
        companies' = D.companies appState
        newAppState = case (parseSafely $ head params) of
          Just(companyId') | isJust $ lookup companyId' companies' -> let
            companyUpkeeps = filter (\(_,u) -> case u of
              U.Upkeep _ ((UM.UpkeepMachine _ machineId) : _) -> 
                case lookup machineId (D.machines appState) of
                  Just(M.Machine _ companyId'' _ _ _) -> companyId'' == companyId'
                  _ -> False
              _ -> False
              ) (D.upkeeps appState)
            in D.UpkeepHistory companyUpkeeps
          _ -> D.NotFound
      modify appVar (\appState -> appState { D.navigation = newAppState })
  ),(
    "machines/:id", \params -> let
      modify' newState = modify appVar (\appState -> appState { D.navigation = newState })
      maybeId = parseSafely $ head params
      in case maybeId of
        Just(machineId') -> fetchMachine machineId' (\machine ->
          modify' $ D.MachineDetail machine False False machineId')
        _ -> modify' D.NotFound )]

navigate :: CrmRoute
         -> CrmRouter
         -> Fay ()
navigate (CrmRoute route) (CrmRouter router) = BR.navigate route router

link :: Renderable a
     => Automatic a
     -> CrmRoute
     -> CrmRouter
     -> DOMElement
link children (CrmRoute route) (CrmRouter router) = 
  BR.link children route router
