{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Crm.Page.UpkeepPrint (
  upkeepPrint ) where


import           Data.Text                    (fromString, (<>), intercalate)
import qualified Data.Text                    as T
import           Data.Maybe                   (onJust)
import           Prelude                      hiding (div)
import           FFI                          (Defined(Defined))
import           Data.Var                     (Var, modify)

import           HaskellReact
import qualified HaskellReact.Bootstrap       as B
import qualified HaskellReact.Bootstrap.Table as BT

import           Crm.Helpers                  (displayDate, plusDays)
import           Crm.Component.Form           (nullDropdown, InputState(Editing))
import           Crm.Component.DatePicker     as DP
import qualified Crm.Router                   as R
import qualified Crm.Data.Data                as D

import qualified Crm.Shared.Company           as C
import qualified Crm.Shared.Employee          as E
import qualified Crm.Shared.ContactPerson     as CP
import qualified Crm.Shared.Machine           as M
import qualified Crm.Shared.MachineType       as MT
import qualified Crm.Shared.Upkeep            as U
import qualified Crm.Shared.UpkeepMachine     as UM
import qualified Crm.Shared.ServerRender      as SR
import qualified Crm.Shared.YearMonthDay      as YMD
import qualified Crm.Shared.Task              as T


renderMarkup :: [SR.Markup] -> [DOMElement]
renderMarkup = let
  renderItem :: SR.Markup -> DOMElement
  renderItem (SR.UnorderedList unorderedList) = ul $
    map renderListItem unorderedList
  renderItem (SR.PlainText t) = text2DOM t
  renderItem (SR.Header h) = h5 h
  renderListItem t = li t
  in map renderItem


upkeepPrint :: 
  R.CrmRouter ->
  Var D.AppState ->
            (YMD.YearMonthDay, DP.DatePickerData)
            -> Maybe (E.EmployeeId, [(T.TaskId, T.TaskMarkup)])
            -> [(U.UpkeepMarkup, C.Company, [E.Employee'], [(M.Machine, 
               MT.MachineType, Maybe CP.ContactPerson, (UM.UpkeepMachine, Maybe [SR.Markup]))])]
            -> [(E.EmployeeId, E.Employee)]
            -> DOMElement
upkeepPrint router appVar (date, datePickerData) employeeTasks data' employees = let
  simpleDateControls = [
    R.link "<< včera" (R.dailyPlan (plusDays (-1) date) Nothing) router ,
    text2DOM " " ,
    R.link "zítra >>" (R.dailyPlan (plusDays (1) date) Nothing) router ]
  plansDatePicker = let 
    modifyDay newDay = modify appVar $ \appState -> appState {
      D.navigation = case D.navigation appState of
        (dp @ D.DailyPlan {}) -> dp { D.day = newDay } }
    modifyDPD dpd = modifyDay (date, dpd)
    modifyDate date' = R.navigate (R.dailyPlan date' (fst `onJust` employeeTasks)) router
    in DP.datePicker' Editing datePickerData modifyDPD date modifyDate
  employeeSelect = fst . nullDropdown employees (text2DOM . E.name) (fst `onJust` employeeTasks) $
    \eId -> R.navigate (R.dailyPlan date eId) router
  header = h2 $ "Denní akce - " <> displayDate date
  renderTasks = map $ \(_, task) -> li . renderMarkup . T.description $ task
  tasks = maybe [text2DOM ""] (\(_, tasks') -> (h3 "Další úkoly":) . (:[]) . ul . renderTasks $ tasks') employeeTasks
  displayUpkeep (upkeep, company, employees', machinesData) = div' (class'' ["row", "print-company"]) $
    B.col (B.mkColProps 12) (
      upkeepPrintDataHeader ++
      [h4 "Úkony"] ++
      [generalDescription] ++
      (rdrMachines machinesData))
    where
    generalDescription = div . renderMarkup . U.workDescription $ upkeep
    upkeepPrintDataHeader = [
      h3 (text2DOM . C.companyName $ company) ,
      BT.table (Just BT.Bordered) [
        tr [th "Adresa", td (text2DOM . C.companyAddress $ company)] ,
        tr [th "Posádka", td . text2DOM . T.intercalate " + " . map renderEmployee $ employees']]]
    renderEmployee (_, employee) = E.name employee
    rdrMachines = map $ \(machine, machineType, contactPerson, (upkeepMachine, markup')) -> let
      upkeepMachineText = case markup' of
        Just (markup) -> div . renderMarkup $ markup
        Nothing -> text2DOM . UM.upkeepMachineNote $ upkeepMachine
      in BT.table (Just BT.Bordered) [
        tr [th "Zařízení", td . text2DOM . MT.machineTypeName $ machineType ] ,
        tr [th "Sériové číslo", td . text2DOM . M.serialNumber $ machine ] ,
        tr [th "Označení", td . text2DOM . M.label_ $ machine ] ,
        tr [th "Kontaktní osoba", td . text2DOM . (maybe "---" CP.name) $ contactPerson ] ,
        tr [th "Telefon", td . text2DOM . (maybe "---" CP.phone) $ contactPerson ] ,
        tr . (td'' mkAttrs (mkTableCellAttributes { colSpan = Defined 2 } )) $ upkeepMachineText ]
  in B.grid $
    (B.row . B.col (B.mkColProps 12) $ header) :
    (B.row' (const . class' $ "no-print") $ [
      B.col (B.mkColProps 6) employeeSelect ,
      B.col (B.mkColProps 3) plansDatePicker ]) :
    map displayUpkeep data' ++
    [B.row . B.col (B.mkColProps 12) $ tasks]
