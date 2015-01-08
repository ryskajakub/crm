{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Page.Other.MachineType (
  machineTypesList ,
  machineTypeForm ) where

import "fay-base" Data.Text (fromString, unpack, pack, showInt)
import "fay-base" Prelude hiding (div, span, id)
import "fay-base" Data.Var (Var, modify)

import HaskellReact
import qualified HaskellReact.Bootstrap as B

import qualified Crm.Router as R
import qualified Crm.Data as D
import qualified Crm.Shared.MachineType as MT
import Crm.Helpers (formRow', parseSafely, saveButtonRow)
import Crm.Server (updateMachineType)

machineTypeForm :: Var D.AppState
                -> MT.MachineType'
                -> (DOMElement, Fay ())
machineTypeForm appVar (machineTypeId, machineType) =
  let 
    setMachineType :: MT.MachineType -> Fay ()
    setMachineType machineType' = modify appVar (\appState -> appState {
      D.navigation = case D.navigation appState of
        D.MachineTypeEdit _ -> D.MachineTypeEdit (machineTypeId, machineType')
        x -> x })
    saveButtonHandler :: Fay ()
    saveButtonHandler = updateMachineType (machineTypeId, machineType) (return ())
    html = main $ B.grid [
      formRow' 
        "Typ zařízení" 
        (MT.machineTypeName machineType)
        (eventString >=> (\string -> setMachineType ( machineType { MT.machineTypeName = string })))
        True ,
      formRow'
        "Výrobce"
        (MT.machineTypeManufacturer machineType)
        (eventString >=> (\string -> setMachineType (machineType { MT.machineTypeManufacturer = string })))
        True ,
      formRow'
        "Interval servisu"
        (unpack $ showInt $ MT.upkeepPerMileage machineType)
        (eventValue >=> (\str -> case parseSafely str of
          Just(int) -> setMachineType (machineType { MT.upkeepPerMileage = int })
          Nothing -> return ()))
        True, 
      saveButtonRow
        "Editovat"
        saveButtonHandler ]
  in (html, return ())

machineTypesList :: R.CrmRouter
                 -> [(MT.MachineType', Int)]
                 -> DOMElement
machineTypesList router machineTypes = let
  head' =
    thead $ tr [
      th "Název typu" , 
      th "Výrobce" , 
      th "Počet zařízení v systému" ]
  body = tbody $ map (\((machineTypeId,(MT.MachineType name manufacturer _)), count) ->
    tr [
      td $ R.link (pack name) (R.machineTypeEdit machineTypeId) router ,
      td $ pack manufacturer , 
      td $ showInt count ]) machineTypes
  in main [
    section $
      B.table [
        head' , 
        body ] ]
