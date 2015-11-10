{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Crm.Page.UpkeepPrint (
  upkeepPrint ) where

import           Data.Text                    (fromString, (<>))
import           Prelude                      hiding (div)

import           HaskellReact
import qualified HaskellReact.Bootstrap       as B
import qualified HaskellReact.Bootstrap.Table as BT

import           Crm.Helpers                  (displayDate, plusDays)
import           Crm.Component.Form           (nullDropdown)
import qualified Crm.Router                   as R

import qualified Crm.Shared.Company           as C
import qualified Crm.Shared.Employee          as E
import qualified Crm.Shared.ContactPerson     as CP
import qualified Crm.Shared.Machine           as M
import qualified Crm.Shared.MachineType       as MT
import qualified Crm.Shared.Upkeep            as U
import qualified Crm.Shared.UpkeepMachine     as UM
import qualified Crm.Shared.ServerRender      as SR
import qualified Crm.Shared.YearMonthDay      as YMD

upkeepPrint :: R.CrmRouter
            -> YMD.YearMonthDay
            -> Maybe E.EmployeeId
            -> [(U.Upkeep, C.Company, [E.Employee'], [(M.Machine, 
               MT.MachineType, CP.ContactPerson, (UM.UpkeepMachine, Maybe [SR.Markup]))])]
            -> [(E.EmployeeId, E.Employee)]
            -> DOMElement
upkeepPrint router day employeeId data' employees = let
  simpleDateControls = [
    R.link "<< včera" (R.dailyPlan (plusDays (-1) day) Nothing) router ,
    text2DOM " " ,
    R.link "zítra >>" (R.dailyPlan (plusDays (1) day) Nothing) router ]
  employeeSelect = fst . nullDropdown employees (text2DOM . E.name) employeeId $
    \eId -> R.navigate (R.dailyPlan day eId) router
  header = h2 $ "Denní akce - " <> displayDate day
  displayUpkeep (_, company, _, machinesData) = div' (class'' ["row", "print-company"]) $
    (B.col (B.mkColProps 12) upkeepPrintDataHeader) :
    (concat . rdrMachines $ machinesData) 
    where
    upkeepPrintDataHeader = BT.table (Just BT.Bordered) [
      tr [th "Firma", td (text2DOM . C.companyName $ company)] ,
      tr [th "Adresa", td (text2DOM . C.companyAddress $ company)]]
    rdrMachines = map $ \(machine, machineType, contactPerson, (upkeepMachine, markup')) -> let
      upkeepMachineText = case markup' of
        Just (markup) -> div . render $ markup
        Nothing -> text2DOM . UM.upkeepMachineNote $ upkeepMachine
      render = map renderItem
      renderItem :: SR.Markup -> DOMElement
      renderItem (SR.UnorderedList unorderedList) = ul $
        map renderListItem unorderedList
      renderItem (SR.PlainText t) = text2DOM t
      renderListItem t = li t
      in map (B.col (B.mkColProps 6)) [
        strong "Zařízení", text2DOM $ MT.machineTypeName machineType <> " " <> M.note machine <> " " <> M.serialNumber machine ,
        strong "Kontaktní osoba", text2DOM $ CP.name contactPerson <> " " <> CP.phone contactPerson] ++ [
        (B.col (B.mkColProps 12) upkeepMachineText) ]
  in B.grid $
    (B.row . B.col (B.mkColProps 12) $ header) :
    (B.row $ [
      B.col (B.mkColProps 6) employeeSelect ,
      B.col (B.mkColProps 6) simpleDateControls ]) :
    map displayUpkeep data' 
