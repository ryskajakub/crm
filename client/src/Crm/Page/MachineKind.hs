{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE MultiWayIf #-}

module Crm.Page.MachineKind (machineKindSettings) where

import           Data.Text                    (fromString)
import           Prelude                      hiding (div, span, id)
import qualified Prelude                      
import           Data.Var                     (Var, modify)
import           Data.Maybe                   (fromJust)

import           HaskellReact
import qualified HaskellReact.Bootstrap       as B
import qualified HaskellReact.BackboneRouter  as BR
import qualified HaskellReact.Bootstrap.Alert as A

import qualified Crm.Shared.MachineKind       as MK
import qualified Crm.Shared.ExtraField        as EF

import qualified Crm.Data.Data                as D
import           Crm.Helpers
import           Crm.Server                   (saveExtraFieldSettings)
import           Crm.Component.Form


machineKindSettings :: Var D.AppState
                    -> Int
                    -> Bool
                    -> MK.MachineKindEnum
                    -> [(MK.MachineKindEnum, [(EF.ExtraFieldIdentification, MK.MachineKindSpecific)])]
                    -> DOMElement
machineKindSettings appVar counter showSuccess editedEnum allSettings = mkGrid where

  machineKindName = fromJust $ lookup editedEnum MK.machineKinds
  theEditedMachineKind = fromJust $ lookup editedEnum allSettings

  mkGrid = div [
    B.grid header, 
    div' (class'' ["container", "form-horizontal"]) $ 
      (machineKindDropdown : kindAttributeFields) ++ [
        B.row submitRow , 
        B.row saveSuccess ]]
    where
    header = pageInfo "Další políčka u strojů" $ Just "Tady můžeš vybrat, jaká další políčka se budou dát vyplnit u strojů. Ke každému druhu stroje můžeš přiřadit další políčka, ty se zobrazí potom na stránce stroje, kde ho vyplníš."
    machineKindDropdown = dropdownRow Editing "Druh stroje" MK.machineKinds (\x -> x) machineKindName
      $ \selectedKind -> D.modifyState appVar $ \navig -> navig { D.editedKind = selectedKind }
    kindAttributeFields = multipleInputs
      "Pole" "Přidat pole" OrderingVisible setList mkInput elems newElem where
        elems = theEditedMachineKind
        newElem = (EF.ToBeAssigned, MK.newMachineKindSpecific)
        setList as = setNewSettings (editedEnum, as)
        mkInput (rowId, rowValue) setRow = input
          Editing
          True 
          (SetValue . MK.name $ rowValue)
          (\t -> setRow $ (rowId, rowValue { MK.name = t }))
    afterTimeout = let
      modify' f = modify appVar $ \appState -> case D.navigation appState of
        ef @ D.ExtraFields {} -> appState { D.navigation = f ef }
      in modify' $ \ef -> ef
    submitRow = buttonRow "Ulož" $ saveExtraFieldSettings allSettings $
      setTimeout 3000 afterTimeout
    saveSuccess = B.col (B.mkColProps 12) $ A.alert A.Success
      "Uloženo"

  setNewSettings :: (MK.MachineKindEnum, [(EF.ExtraFieldIdentification, MK.MachineKindSpecific)]) -> Fay ()
  setNewSettings (key', newFields) = let
    newAllSettings = (key', newFields) : filter (\(e,_) -> e /= key') allSettings
    in D.modifyState appVar $ \navig -> navig { D.allSettings = newAllSettings }
