{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Page.Other.MachineType (
  machineTypesList ,
  machineTypeForm ) where

import "fay-base" Data.Text (fromString, unpack, pack, showInt)
import "fay-base" Prelude hiding (div, span, id)
import Data.Var (Var)

import HaskellReact
import qualified HaskellReact.Bootstrap as B

import qualified Crm.Router as R
import qualified Crm.Data as D
import qualified Crm.Shared.MachineType as MT
import Crm.Helpers (formRow)

machineTypeForm :: Var D.AppState
                -> MT.MachineType'
                -> (DOMElement, Fay ())
machineTypeForm appVar (machineTypeId, machineType) =
  let 
    html = main $ B.grid [
      formRow "Text" "Text" ]
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
