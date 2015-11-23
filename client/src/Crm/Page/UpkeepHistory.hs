{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Crm.Page.UpkeepHistory (
  upkeepHistory ) where

import           Data.Text                     (fromString, showInt, (<>), intercalate)
import           Prelude                       hiding (div, span, id, intercalate)
import           FFI                           (Defined(Defined))

import           HaskellReact
import qualified HaskellReact.Bootstrap        as B
import qualified HaskellReact.Bootstrap.Nav    as BN
import qualified HaskellReact.Bootstrap.Button as BTN
import qualified HaskellReact.BackboneRouter   as BR

import qualified Crm.Shared.Upkeep             as U
import qualified Crm.Shared.UpkeepMachine      as UM
import qualified Crm.Shared.Machine            as M
import qualified Crm.Shared.MachineType        as MT
import qualified Crm.Shared.Employee           as E
import qualified Crm.Shared.Company            as C
import           Crm.Helpers                   (displayDate)
import           Crm.Router
import           Crm.Server                    (deleteUpkeep)


upkeepHistory :: 
  [(U.UpkeepId, U.Upkeep, [(UM.UpkeepMachine, MT.MachineType, M.MachineId)], [E.Employee'])] -> 
  C.CompanyId -> 
  CrmRouter -> 
  DOMElement
upkeepHistory upkeepsInfo companyId router = let

  upkeepRenderHtml (upkeepId, upkeep, upkeepMachines, employees) = [generalUpkeepInfo, notes, upkeepMachinesInfo] where

    generalUpkeepInfo = B.row' marginTop [
      B.col (B.mkColProps 4) [
        text2DOM $ (<> " ") $ displayDate $ U.upkeepDate upkeep ,
        span' (class'' ["label", "upkeep-state" , labelClass]) labelText ] ,
      let 
        i = if length employees > 1 then "i" else ""
        in B.col (B.mkColProps 4) [ strong $ "Servisman" <> i <> ": ", text2DOM employeeText ] ,
      B.col (B.mkColProps 2) $ formLink , 
      B.col (B.mkColProps 2) $ deleteButton ] where
        employeeText = intercalate " " . map (\(_, employee) -> E.name employee) $ employees
        (labelClass, labelText, formLink) = if U.upkeepClosed upkeep
          then ("label-success", "Uzavřený", text2DOM "")
          else ("label-warning", "Naplánovaný", link "Uzavřít" (upkeepDetail upkeepId) router)
        marginTop attributes = let
          previousClassname = className attributes
          newClassname = case previousClassname of
            Defined text -> Defined $ text <> " upkeep-row"
            _ -> Defined "upkeep-row"
          in attributes { className = newClassname }
        deleteButton = let
          clickHandler = deleteUpkeep upkeepId BR.refresh router
          buttonProps = (BTN.buttonProps {
            BTN.bsStyle = Defined "danger" ,
            BTN.onClick = Defined $ const clickHandler })
          in span' (class' "delete") $ BTN.button' buttonProps "Smazat"

    notes = B.row [
      mkCol note , 
      mkCol recommendation ] where
        mkCol = B.col (B.mkColProps 6)
        note = [
          h3 "Popis servisu" ,
          noteContent ]
        recommendation = [
          h3 "Doporučení" ,
          recommendationContent ]
        noteContent = text2DOM ""
        recommendationContent = text2DOM ""

    mkLineUpkeepMachineInfo (upkeepMachine, machineType, machineId) =
      B.col (B.mkColProps 4) $ B.panel [ h3 $ link 
        (MT.machineTypeName machineType)
        (machineDetail machineId)
        router ,
        dl [[
          dt "Plánované úkony" ,
          dd $ UM.upkeepMachineNote upkeepMachine ,
          dt "Závěry po servisu" ,
          dd $ UM.endNote upkeepMachine ] ++ 
          (if U.upkeepClosed upkeep then [
          dt "Naměřené motohodiny" ,
          dd $ showInt $ UM.recordedMileage upkeepMachine ,
          dd "Záruka" ,
          dd $ (if UM.warrantyUpkeep upkeepMachine then "Ano" else "Ne") ] else []) ]]
    upkeepMachinesInfo = B.row $ map mkLineUpkeepMachineInfo upkeepMachines

  upkeepsHtml = map upkeepRenderHtml upkeepsInfo
  flattenedUpkeepsHtml = foldl (++) [] upkeepsHtml
  header = B.row $ B.col (B.mkColProps 12) (h2 "Historie servisů")
  linkToCompany = B.row $ B.col (B.mkColProps 12) $
    BN.nav [ link "Zpátky na firmu" (companyDetail companyId) router ]

  in div $ B.grid (header : linkToCompany : flattenedUpkeepsHtml)
