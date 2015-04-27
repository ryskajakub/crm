{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Page.UpkeepHistory (
  upkeepHistory ) where

import "fay-base" Data.Text (fromString, pack, showInt, (<>))
import "fay-base" Prelude hiding (div, span, id)
import "fay-base" FFI (Defined(Defined))

import HaskellReact
import qualified HaskellReact.Bootstrap as B
import qualified HaskellReact.Bootstrap.Nav as BN
import qualified HaskellReact.Bootstrap.Button as BTN
import qualified HaskellReact.BackboneRouter as BR

import qualified Crm.Shared.Upkeep as U
import qualified Crm.Shared.UpkeepMachine as UM
import qualified Crm.Shared.Machine as M
import qualified Crm.Shared.MachineType as MT
import qualified Crm.Shared.Employee as E
import qualified Crm.Shared.Company as C
import Crm.Helpers (displayDate)
import Crm.Router
import Crm.Server (deleteUpkeep)

upkeepHistory :: [(U.UpkeepId, U.Upkeep, [(UM.UpkeepMachine, MT.MachineType, M.MachineId)], 
                 Maybe E.Employee')]
              -> C.CompanyId
              -> CrmRouter
              -> DOMElement
upkeepHistory upkeepsInfo companyId router = let

  upkeepHtml (upkeepId, upkeep, upkeepMachines, maybeEmployee) = let
    employeeText = maybe ("---") (pack . E.name . snd) maybeEmployee
    (labelClass, labelText, formLink) = if U.upkeepClosed upkeep
      then ("label-success", "Uzavřený", text2DOM "")
      else ("label-warning", "Naplánovaný", link "Uzavřít" (closeUpkeep upkeepId) router)
    marginTop attributes = let
      previousClassname = className attributes
      newClassname = case previousClassname of
        Defined text -> Defined $ text <> " upkeep-row"
        _ -> Defined "upkeep-row"
      in attributes { className = newClassname }
    deleteButton = let
      doDelete = deleteUpkeep upkeepId BR.refresh
      buttonProps = (BTN.buttonProps {
        BTN.bsStyle = Defined "danger" ,
        BTN.onClick = Defined $ const doDelete })
      in span' (class' "delete") $ BTN.button' buttonProps "Smazat"
    in [
      B.row' marginTop [
        B.col (B.mkColProps 4) [
          text2DOM $ (<> " ") $ displayDate $ U.upkeepDate upkeep ,
          span' (class'' ["label", "upkeep-state" , labelClass]) labelText ] ,
        B.col (B.mkColProps 4) [ strong "Servisman: ", text2DOM employeeText ] ,
        B.col (B.mkColProps 2) $ formLink , 
        B.col (B.mkColProps 2) $ deleteButton ] ,
      B.row $ map (\(upkeepMachine, machineType, machineId) ->
        B.col (B.mkColProps 4) $ B.panel [ h3 $ link 
          (pack $ MT.machineTypeName machineType)
          (machineDetail machineId)
          router ,
          dl [[
            dt "Poznámka" ,
            dd $ pack $ UM.upkeepMachineNote upkeepMachine ] ++ 
            (if U.upkeepClosed upkeep then [
            dt "Naměřené motohodiny" ,
            dd $ showInt $ UM.recordedMileage upkeepMachine ,
            dd "Záruka" ,
            dd $ (if UM.warrantyUpkeep upkeepMachine then "Ano" else "Ne") ] else []) ]]) upkeepMachines ]
  upkeepsHtml = map upkeepHtml upkeepsInfo
  flattenedUpkeepsHtml = foldl (++) [] upkeepsHtml
  header = B.row $ B.col (B.mkColProps 12) (h2 "Historie servisů")
  linkToCompany = B.row $ B.col (B.mkColProps 12) $
    BN.nav [ link "Zpátky na firmu" (companyDetail companyId) router ]

  in div $ B.grid (header : linkToCompany : flattenedUpkeepsHtml)
