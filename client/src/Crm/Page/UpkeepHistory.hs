{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Page.UpkeepHistory (
  upkeepHistory ) where

import "fay-base" Data.Text (fromString, pack, showInt)
import "fay-base" Prelude hiding (div, span, id)

import HaskellReact
import qualified HaskellReact.Bootstrap as B

import qualified Crm.Shared.Upkeep as U
import qualified Crm.Shared.UpkeepMachine as UM
import qualified Crm.Shared.Machine as M
import qualified Crm.Shared.MachineType as MT
import qualified Crm.Shared.Employee as E
import Crm.Helpers (displayDate)
import Crm.Router

upkeepHistory :: [(U.UpkeepId, U.Upkeep, [(UM.UpkeepMachine, MT.MachineType, M.MachineId)], 
                 Maybe E.Employee')]
              -> CrmRouter
              -> DOMElement
upkeepHistory upkeepsInfo router = let
  upkeepHtml (_, upkeep, upkeepMachines, maybeEmployee) = let
    employeeText = maybe ("---") (pack . E.name . snd) maybeEmployee
    in [
      B.row $ B.col (B.mkColProps 12) (h3 $ displayDate $ U.upkeepDate upkeep) ,
      B.row $ B.col (B.mkColProps 12) [
        p [ strong "Servisman", text2DOM " " , text2DOM employeeText ] ,
        p [ strong "Uzavřeno", text2DOM " " ,
          text2DOM $ if U.upkeepClosed upkeep then "Ano" else "Ne" ]] ,
      B.row $ map (\(upkeepMachine, machineType, machineId) ->
        B.col (B.mkColProps 4) $ B.panel [ h2 $ link 
          (pack $ MT.machineTypeName machineType)
          (machineDetail machineId)
          router ,
          dl [
            dt "Poznámka" ,
            dd $ pack $ UM.upkeepMachineNote upkeepMachine ,
            dt "Naměřené motohodiny" ,
            dd $ showInt $ UM.recordedMileage upkeepMachine ]]) upkeepMachines ]
  in div [
    h2 "Historie servisů" ,
    B.grid $ map upkeepHtml upkeepsInfo ]
