{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Crm.Page.UpkeepPrint (
  upkeepPrint ) where

import           Data.Text                (fromString, (<>))
import           Prelude                  hiding (div)

import           HaskellReact
import qualified HaskellReact.Bootstrap   as B

import qualified Crm.Shared.Company       as C
import qualified Crm.Shared.Employee      as E
import qualified Crm.Shared.ContactPerson as CP
import qualified Crm.Shared.Machine       as M
import qualified Crm.Shared.MachineType   as MT
import qualified Crm.Shared.Upkeep        as U
import qualified Crm.Shared.UpkeepMachine as UM
import qualified Crm.Shared.ServerRender  as SR

upkeepPrint :: [(U.Upkeep, C.Company, [E.Employee], [(M.Machine, 
               MT.MachineType, CP.ContactPerson, (UM.UpkeepMachine, Maybe [SR.Markup]))])]
            -> DOMElement
upkeepPrint data' = let
  header = h2 "Denní akce"
  displayUpkeep (_, company, _, machinesData) = div' (class'' ["row", "print-company"]) $
    upkeepPrintDataHeader ++ 
    (concat . rdrMachines $ machinesData) 
    where
    upkeepPrintDataHeader = map (B.col (B.mkColProps 6)) [
      strong "Firma" , text2DOM . C.companyName $ company ,
      strong "Adresa" , text2DOM . C.companyAddress $ company ]
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
      machineTitle = strong . MT.machineTypeName $ machineType
      in map (B.col (B.mkColProps 6)) [
        strong "Zařízení", text2DOM $ MT.machineTypeName machineType <> " " <> M.note machine <> " " <> M.serialNumber machine ,
        strong "Kontaktní osoba", text2DOM $ CP.name contactPerson <> " " <> CP.phone contactPerson] ++ [
        (B.col (B.mkColProps 12) machineTitle) ,
        (B.col (B.mkColProps 12) upkeepMachineText) ]
  in B.grid $
    (B.row $ B.col (B.mkColProps 12) header) :
    map displayUpkeep data' 
