{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE MultiWayIf #-}

module Crm.Page.MachineKind (machineKindSettings) where

import Data.Text (fromString, showInt, (<>))
import Prelude hiding (div, span, id)
import Data.Var (Var)
import Data.Maybe (fromJust)
import FFI (Defined(Defined))

import HaskellReact
import qualified HaskellReact.Bootstrap as B
import qualified HaskellReact.Tag.Hyperlink as A
import qualified HaskellReact.Bootstrap.Glyphicon as G
import qualified HaskellReact.Bootstrap.Button as BTN
import qualified HaskellReact.BackboneRouter as BR

import qualified Crm.Shared.MachineKind as MK
import qualified Crm.Shared.ExtraField as EF

import qualified Crm.Data.Data as D
import Crm.Helpers
import Crm.Server (saveExtraFieldSettings)
import Crm.Component.Form

data FieldPosition = First | Last | Single | Middle


machineKindSettings :: Var D.AppState
                    -> MK.MachineKindEnum
                    -> [(MK.MachineKindEnum, [(EF.ExtraFieldIdentification, MK.MachineKindSpecific)])]
                    -> DOMElement
machineKindSettings appVar editedEnum allSettings = let

  select = maybeSelectRow' False True "Druh stroje" MK.machineKinds (\x -> x) (Just editedEnum)
    (\selectedKind -> D.modifyState appVar $ \navig -> navig { D.editedKind = fromJust selectedKind })
    (const undefined)

  theEditedMachineKind = fromJust $ lookup editedEnum allSettings

  setNewSettings :: (MK.MachineKindEnum, [(EF.ExtraFieldIdentification, MK.MachineKindSpecific)]) -> Fay ()
  setNewSettings (key', newFields) = let
    newAllSettings = (key', newFields) : filter (\(e,_) -> e /= key') allSettings
    in D.modifyState appVar $ \navig -> navig { D.allSettings = newAllSettings }

  displayRow (index, positionInOrdering, (_, extraFieldData)) = let

    changeOrder :: Bool -> [(EF.ExtraFieldIdentification, MK.MachineKindSpecific)]
    changeOrder down = let
      (start, (y:x:rest)) = splitAt (if down then index else index - 1) theEditedMachineKind
      in start ++ (x:y:rest)
    downArrowLink = A.a''' (click $ setNewSettings (editedEnum, changeOrder True)) G.arrowDown
    downArrow = case positionInOrdering of
      Middle -> [downArrowLink]
      First -> [downArrowLink]
      _ -> []
    upArrowLink = A.a''' (click $ setNewSettings (editedEnum, changeOrder False)) G.arrowUp 
    upArrow = case positionInOrdering of
      Middle -> [upArrowLink]
      Last -> [upArrowLink]
      _ -> []

    controls = div' (class'' ["col-md-1", "control-label"]) $ downArrow ++ upArrow
    fieldLabel = label' (class'' ["control-label", "col-md-2"]) ("Pole " <> showInt index)
    setFieldName string = let
      (start, (fieldId,field):rest) = splitAt index theEditedMachineKind
      modifiedX = (fieldId, field { MK.name = string })
      newFields = start ++ [modifiedX] ++ rest
      in setNewSettings (editedEnum, newFields)
    theInput = div' (class' "col-md-7") $ editingInput True (SetValue $ MK.name extraFieldData)
      (eventValue >=> setFieldName) True

    removeButton = let
      removeField = let
        (start, _:rest) = splitAt index theEditedMachineKind
        newSettings = start ++ rest
        in setNewSettings (editedEnum, newSettings)
      props = BTN.buttonProps {
        BTN.bsStyle = Defined "danger" ,
        BTN.onClick = Defined $ const removeField }
      buttonLabel = "Odeber"
      button = BTN.button' props buttonLabel
      in B.col (B.mkColProps 2) button

    in div' ((class' "form-group") { key = Defined $ "key-" <> showInt index }) [
      controls ,
      fieldLabel ,
      theInput ,
      removeButton ]

  submitRow = saveButtonRow "Ulož" $ saveExtraFieldSettings allSettings BR.refresh

  addAnotherFieldButton = let
    addField = let
      newField = (EF.ToBeAssigned, MK.newMachineKindSpecific)
      in setNewSettings (editedEnum, theEditedMachineKind ++ [newField])
    props =  BTN.buttonProps { BTN.onClick = Defined $ const addField }
    buttonLabel = "Přidat políčko"
    in BTN.button' props buttonLabel
  addAnotherRow = labeledRowOneElement addAnotherFieldButton ""

  lastIndex = length theEditedMachineKind - 1
  assignPosition (i, field) = if 
    | i == 0 && i == lastIndex -> (i, Single, field)
    | i == lastIndex -> (i, Last, field)
    | i == 0 -> (i, First, field)
    | True -> (i, Middle, field)
  fieldsWithPositions = map assignPosition $ zipWithIndex theEditedMachineKind
  inputFieldRows = map displayRow fieldsWithPositions

  header = pageInfo "Další políčka u strojů" $ Just "Tady můžeš vybrat, jaká další políčka se budou dát vyplnit u strojů. Ke každému druhu stroje můžeš přiřadit další políčka, ty se zobrazí potom na stránce stroje, kde ho vyplníš."
  in div [B.grid header, div' (class'' ["container", "form-horizontal"]) $ (select : inputFieldRows) ++ [B.row addAnotherRow ,B.row submitRow]]
