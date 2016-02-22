{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Crm.Server.Database.MachineType where

import           Opaleye                         (required, Table(..), optional)
import           Data.Profunctor.Product.TH      (makeAdaptorAndInstance')
import           Control.Lens                    (makeLenses)

import qualified Crm.Shared.MachineType          as MT
import           Crm.Server.Database.PrimaryKeys
import           Crm.Server.Database.Types

data MachineTypeRow' machineTypePK machineType = MachineTypeRow {
  _machineTypePK :: machineTypePK ,
  _machineType :: machineType }
makeLenses ''MachineTypeRow'
makeAdaptorAndInstance' ''MachineTypeRow'

type MachineTypeDBRow pk = MachineTypeRow' pk (MT.MachineType' DBInt DBText DBText)
type MachineTypesWriteTable = MachineTypeDBRow (MT.MachineTypeId' (Maybe DBInt))
type MachineTypesTable = MachineTypeDBRow MachineTypePK
type MachineTypeRecord = MachineTypeRow' MT.MachineTypeId MT.MachineType

machineTypesTable :: Table MachineTypesWriteTable MachineTypesTable
machineTypesTable = Table "machine_types" $ pMachineTypeRow MachineTypeRow {
  _machineTypePK = MT.pMachineTypeId . MT.MachineTypeId . optional $ "id" ,
  _machineType = MT.pMachineType MT.MachineType {
    MT.kind = required "machine_kind" ,
    MT.machineTypeName = required "name" ,
    MT.machineTypeManufacturer = required "manufacturer" }}
