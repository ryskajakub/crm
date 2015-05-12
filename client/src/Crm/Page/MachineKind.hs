{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Page.MachineKind (machineKindSettings) where

import "fay-base" Data.Text (fromString, unpack, pack, showInt, (<>), Text)
import "fay-base" Prelude hiding (div, span, id)
import "fay-base" Data.Var (Var, modify)

import HaskellReact

import qualified Crm.Shared.MachineType as MT
import qualified Crm.Shared.MachineKind as MK

import qualified Crm.Router as R
import qualified Crm.Data.Data as D
import Crm.Component.Form

machineKindSettings :: Var D.AppState
                    -> MK.MachineKindEnum
                    -> [(MK.MachineKindEnum, [MK.MachineKindSpecific])]
                    -> DOMElement
machineKindSettings appVar editedEnum allSettings = div ""
