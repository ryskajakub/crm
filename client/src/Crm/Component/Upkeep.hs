{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Component.Upkeep (
  upkeepNew
) where

import HaskellReact as HR
import qualified Crm.Shared.Company as C
import qualified Crm.Shared.Machine as M
import qualified Crm.Shared.MachineType as MT
import qualified Crm.Shared.Upkeep as U
import qualified Crm.Shared.UpkeepMachine as UM
import "fay-base" Data.Text (fromString, unpack, pack, append, showInt)
import "fay-base" Prelude hiding (div, span, id)
import Data.Var (Var, modify)
import FFI (Defined(Defined))
import HaskellReact.BackboneRouter (link, navigate)
import qualified HaskellReact.Bootstrap as B
import qualified HaskellReact.Bootstrap.Input as I
import qualified HaskellReact.Bootstrap.Button as BTN
import qualified HaskellReact.Bootstrap.Glyphicon as G
import qualified HaskellReact.Tag.Input as II
import qualified HaskellReact.Tag.Hyperlink as A
import Crm.Component.Data
import Crm.Component.Editable (editable)
import Crm.Server (createMachine)

import Debug.Trace

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

{- | if the element is in the first list, put it in the other one, if the element
 - is in the other, put in the first list
 -}
toggle :: ([a],[a]) -> (a -> Bool) -> ([a],[a])
toggle lists findElem = let 
  toggleInternal (list1, list2) runMore = let
    foundInFirstList = find findElem list1
    in case foundInFirstList of 
      Just(elem) -> let
        filteredList1 = filter (not . findElem) list1
        addedToList2 = elem : list2
        result = (filteredList1, addedToList2)
        in if runMore then result else swap result
      _ -> if runMore
        then toggleInternal (list2, list1) False
        else lists
  in toggleInternal lists True

upkeepNew :: MyData
          -> Var AppState
          -> U.Upkeep
          -> [UM.UpkeepMachine]
          -> [(Int, M.Machine)] -- ^ machine ids -> machines
          -> DOMElement
upkeepNew myData appState upkeep' notCheckedMachines'' machines = let
  setUpkeep :: U.Upkeep -> Maybe [UM.UpkeepMachine] -> Fay()
  setUpkeep upkeep' notCheckedMachines' = modify appState (\appState' -> case navigation appState' of
    upkeepNew @ (UpkeepNew _ _ _) -> let
      newNavigation = upkeepNew { upkeep = upkeep' }
      newNavigation' = case notCheckedMachines' of 
        Just(x) -> newNavigation { notCheckedMachines = x }
        _ -> newNavigation
      in appState' { navigation = newNavigation' } )
  machineRow (machineId, machine) = let
    checkedMachineIds = map (UM.upkeepMachineMachineId) (U.upkeepMachines upkeep')
    rowProps = if elem machineId checkedMachineIds
      then class' "bg-success"
      else mkAttrs
    in B.row' rowProps [
      let
        content = span $ pack $ (MT.machineTypeName . M.machineType) machine
        clickHandler = let
          (newCheckedMachines, newNotCheckedMachines) = toggle (
            U.upkeepMachines upkeep' , 
            notCheckedMachines'' ) 
            (\(UM.UpkeepMachine _ machineId') -> machineId' == machineId)
          newUpkeep = upkeep' { U.upkeepMachines = newCheckedMachines }
          in setUpkeep newUpkeep $ Just newNotCheckedMachines
        link = A.a''
          (mkAttrs{onClick = Defined $ const clickHandler})
          (A.mkAAttrs)
          content
        in B.col (B.mkColProps 6) link ,
      B.col (B.mkColProps 6) $ I.textarea (I.mkInputProps {
        I.onChange = Defined $ const $ return () })]
  submitButton = let
    newUpkeepHandler = return ()
    buttonProps = BTN.buttonProps {
      BTN.bsStyle = Defined "primary" ,
      BTN.onClick = Defined $ const newUpkeepHandler }
    button = BTN.button' buttonProps [ G.plus , text2DOM " Naplánovat" ]
    in B.col ((B.mkColProps 6){ B.mdOffset = Defined 6 }) button
  dateRow = B.row [
    B.col (B.mkColProps 6) "Datum" ,
    B.col (B.mkColProps 6) $ I.input (I.mkInputProps)]
  in div $
    B.grid $
      map machineRow machines ++ [dateRow, submitButton]
