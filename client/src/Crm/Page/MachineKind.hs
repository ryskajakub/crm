{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiWayIf #-}

module Crm.Page.MachineKind (machineKindSettings) where

import "fay-base" Data.Text (fromString, unpack, pack, showInt, (<>), Text)
import "fay-base" Prelude hiding (div, span, id)
import "fay-base" Data.Var (Var, modify)
import "fay-base" Data.Maybe (fromJust)
import "fay-base" FFI (Defined(Defined))

import HaskellReact
import qualified HaskellReact.Bootstrap as B

import qualified Crm.Shared.MachineType as MT
import qualified Crm.Shared.MachineKind as MK
import qualified Crm.Shared.ExtraField as EF

import qualified Crm.Router as R
import qualified Crm.Data.Data as D
import Crm.Helpers
import Crm.Component.Form

data FieldPosition = First | Last | Single | Middle


machineKindSettings :: Var D.AppState
                    -> MK.MachineKindEnum
                    -> [(MK.MachineKindEnum, [(EF.ExtraFieldIdentification, MK.MachineKindSpecific)])]
                    -> DOMElement
machineKindSettings appVar editedEnum allSettings = let

  select = maybeSelectRow' False True "Druh stroje" MK.machineKinds pack (Just editedEnum)
    (\selectedKind -> D.modifyState appVar $ \navig -> navig { D.editedKind = fromJust selectedKind })
    (const undefined)

  theEditedMachineKind = fromJust $ lookup editedEnum allSettings

  displayRow (index, _, (extraFieldIdentification, extraFieldData)) = let
    controls = div' (class' "col-md-1") ""
    fieldLabel = label' (class'' ["control-label", "col-md-2"]) ("Pole " <> showInt index)
    setFieldName string = let
      (start, (fieldId,field):rest) = splitAt index theEditedMachineKind
      modifiedX = (fieldId, field { MK.name = string })
      newFields = start ++ [modifiedX] ++ rest
      newAllSettings = (editedEnum, newFields) : filter (\(e,_) -> e /= editedEnum) allSettings
      in D.modifyState appVar $ \navig -> navig { D.allSettings = newAllSettings }
    theInput = div' (class' "col-md-9") $ editingInput True (SetValue $ MK.name extraFieldData) 
      (eventString >=> setFieldName) True
    in div' ((class' "form-group") { key = Defined $ "key-" <> showInt index }) [
      fieldLabel ,
      controls ,
      theInput ]

  lastIndex = length theEditedMachineKind - 1
  assignPosition (i, field) = if 
    | i == 0 && i == lastIndex -> (i, Single, field)
    | i == lastIndex -> (i, Last, field)
    | i == 0 -> (i, First, field)
    | True -> (i, Middle, field)
  fieldsWithPositions = map assignPosition $ zipWithIndex theEditedMachineKind
  inputFieldRows = map displayRow fieldsWithPositions

  header = pageInfo "Další políčka u strojů" $ Just "Tady můžeš vybrat, jaká další políčka se budou dát vyplnit u strojů. Ke každému druhu stroje můžeš přiřadit další políčka, ty se zobrazí potom na stránce stroje, kde ho vyplníš."
  in div [B.grid header, div' (class'' ["container", "form-horizontal"]) $ select : inputFieldRows]
