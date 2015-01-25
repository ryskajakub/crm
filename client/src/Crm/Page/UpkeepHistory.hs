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
  upkeepHtml (upkeepId, upkeep, upkeepMachines, maybeEmployee) = let
    employeeText = maybe ("---") (pack . E.name . snd) maybeEmployee
    formLink = if U.upkeepClosed upkeep
      then text2DOM ""
      else link "Uzavřít" (closeUpkeep upkeepId) router
    in [
      B.row $ B.col (B.mkColProps 12) (div' (class' "relative") [
        p (displayDate $ U.upkeepDate upkeep) ,
        div' (class' "same-line") $ p' (class' "text-center") [strong "Servisman: ", text2DOM employeeText] ,
        div' (class' "same-line") $ div' (class' "text-right") formLink ]) ,
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
  in div $ B.grid ([B.row $ B.col (B.mkColProps 12) (h1 "Historie servisů")] :
    map upkeepHtml upkeepsInfo)
