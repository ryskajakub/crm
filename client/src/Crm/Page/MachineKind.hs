{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Page.MachineKind (machineKindSettings) where

import "fay-base" Data.Text (fromString, unpack, pack, showInt, (<>), Text)
import "fay-base" Prelude hiding (div, span, id)
import "fay-base" Data.Var (Var, modify)
import "fay-base" Data.Maybe (fromJust)

import HaskellReact
import qualified HaskellReact.Bootstrap as B

import qualified Crm.Shared.MachineType as MT
import qualified Crm.Shared.MachineKind as MK

import qualified Crm.Router as R
import qualified Crm.Data.Data as D
import Crm.Helpers
import Crm.Component.Form

mkSetMachineType :: Var D.AppState -> MT.MachineType -> Fay ()
mkSetMachineType appVar modifiedMachineType = 
  D.modifyState appVar (\navig -> navig { D.machineTypeTuple = lmap (const modifiedMachineType) (D.machineTypeTuple navig) })

machineKindSettings :: Var D.AppState
                    -> MK.MachineKindEnum
                    -> [(MK.MachineKindEnum, [MK.MachineKindSpecific])]
                    -> DOMElement
machineKindSettings appVar editedEnum allSettings = let

  select = maybeSelectRow' False True "Druh stroje" MK.machineKinds pack (Just editedEnum) 
    (\selectedKind -> D.modifyState appVar $ \navig -> navig { D.editedKind = fromJust selectedKind })
    (const undefined)

  header = pageInfo "Další políčka u strojů" $ Just "Tady můžeš vybrat, jaká další políčka se budou dát vyplnit u strojů. Ke každému druhu stroje můžeš přiřadit další políčka, ty se zobrazí potom na stránce stroje, kde ho vyplníš."
  in div [B.grid header, B.grid select]
