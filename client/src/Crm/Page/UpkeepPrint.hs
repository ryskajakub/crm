{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Crm.Page.UpkeepPrint (
  upkeepPrint ) where


import           Data.Text                    (fromString, (<>))
import qualified Data.Text                    as T
import           Data.Maybe                   (onJust, joinMaybe, mapMaybe)
import           Prelude                      hiding (div)
import           Data.Var                     (Var, modify)

import           HaskellReact
import qualified HaskellReact.Bootstrap       as B
import qualified HaskellReact.Bootstrap.Table as BT

import           Crm.Helpers                  (displayDate, renderMarkup, displayFullMachine', isMarkupEmpty)
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


upkeepPrint :: 
  R.CrmRouter ->
  Var D.AppState ->
  (YMD.YearMonthDay, DP.DatePickerData) -> 
  Maybe (E.EmployeeId, [(T.TaskId, T.TaskMarkup)]) -> 
  [(U.UpkeepMarkup, C.Company, [E.Employee'], [(M.Machine, MT.MachineType, 
    Maybe CP.ContactPerson, (UM.UpkeepMachine, Maybe [SR.Markup]))])] -> 
  [(E.EmployeeId, E.Employee)] -> 
  DOMElement
upkeepPrint router appVar (date, datePickerData) employeeTasks data' employees = let
  plansDatePicker = let 
    modifyDay newDay = modify appVar $ \appState -> appState {
      D.navigation = case D.navigation appState of
        (dp @ D.DailyPlan {}) -> dp { D.day = newDay } }
    modifyDPD dpd = modifyDay (date, dpd)
    modifyDate date' = R.navigate (R.dailyPlan date' (fst `onJust` employeeTasks)) router
    in DP.datePicker' Editing datePickerData modifyDPD date modifyDate
  employeeSelect = fst . nullDropdown employees (text2DOM . E.name) (fst `onJust` employeeTasks) $
    \eId -> R.navigate (R.dailyPlan date eId) router
  employeeName = joinMaybe ((flip lookup employees . fst) `onJust` employeeTasks)

  machinesTable = B.fullRow $ div [div address , BT.table (Just BT.FullBordered) (
    (tr [th "Datum", td . displayDate $ date]) :
    concatMap displayUpkeep data')]
    where
    address = map (p' (class' "visible-print-block" )) [[text2DOM "2e plus s.r.o."], [text2DOM "Trabantská 270, Praha 9, 190 15"], text2DOM "Tel: 281 917 430" : span' (class' "pad-left") "Fax: 281 917 435" : span' (class' "pad-left") [text2DOM "e-mail: " : text2DOM "info@2e.cz" : [] ] : [] ]

  pluckContactPersons = nub . mapMaybe (\(_,_,cp',_) -> cp')

  header = h2 $ "DENNÍ AKCE - " <> maybe "" E.name employeeName
  renderTasks = map $ \(_, task) -> li . renderMarkup . T.description $ task
  ifNonEmpty [] _ _ = []
  ifNonEmpty _ prepend list = prepend : list
  tasks = maybe [text2DOM ""] (\(_, tasks') -> ifNonEmpty tasks' (h3 "Další úkoly") . (:[]) . ul . renderTasks $ tasks') employeeTasks
  displayUpkeep (upkeep, company, _, machinesData) =
      (tr' (class' "empty-row") . tdColSpan 2 $ "|") :
      upkeepPrintDataHeader ++
      (concat . rdrMachines $ machinesData) ++
      generalDescription
    where
    generalDescription = if (all isMarkupEmpty . U.workDescription $ upkeep)
      then []
      else [tr' (class' "indent-cell") [tdColSpan 2 . renderMarkup . U.workDescription $ upkeep]]
    upkeepPrintDataHeader = let
      ifNonEmpty' toRender = if (null . pluckContactPersons $ machinesData) then [] else toRender
      in [
        tr [th "Firma", td (text2DOM . C.companyName $ company)] ,
        tr [th "Adresa", td (text2DOM . C.companyAddress $ company)]] ++
        ifNonEmpty' [tr [th "Kontaktní osoba", td . text2DOM . T.intercalate ", " . 
          map CP.name . pluckContactPersons $ machinesData]] ++
        ifNonEmpty' [tr [th "Telefon", td . text2DOM . T.intercalate ", " .
          map CP.phone . pluckContactPersons $ machinesData]]
    rdrMachines = map $ \(machine, machineType, _, (upkeepMachine, markup')) -> let
      upkeepMachineText' = case markup' of
        Just (markup) -> (\m -> if (all isMarkupEmpty m) then Nothing else Just . div . renderMarkup $ m) $ markup
        Nothing -> (\x -> if T.null x then Nothing else Just . text2DOM $ x) . UM.upkeepMachineNote $ upkeepMachine
      in maybe [] (\upkeepMachineText -> [tr' (class' "indent-cell") $
        tdColSpan 2 [p $ strong $ displayFullMachine' True machine machineType, p upkeepMachineText]]) upkeepMachineText'
  in B.grid $
    (B.row . B.col (B.mkColProps 12) $ header) :
    (B.row' (const . class' $ "no-print") $ [
      B.col (B.mkColProps 6) employeeSelect ,
      B.col (B.mkColProps 3) plansDatePicker ]) :
    machinesTable :
    [B.row . B.col (B.mkColProps 12) $ tasks]
