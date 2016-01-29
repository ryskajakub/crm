{-# OPTIONS -fno-warn-missing-signatures #-}   

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


module Crm.Server.Database.UpkeepMachine where

import           Opaleye                    --(required, optional, Table (..), queryRunnerColumn,
                                            --  PGInt4, QueryRunnerColumn)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance')
import           Data.Profunctor.Product.Default (Default(def))
import           Control.Lens               (makeLenses, over)

import qualified Crm.Shared.UpkeepMachine             as UM
import qualified Crm.Shared.Machine                   as M
import qualified Crm.Shared.Upkeep                    as U
import           Crm.Server.Database.PrimaryKeys
import           Crm.Server.Database.Types

data UpkeepMachineRow' upkeepFK machineFK upkeepMachine = 
  UpkeepMachineRow {
    _upkeepFK :: upkeepFK ,
    _machineFK :: machineFK ,
    _upkeepMachine :: upkeepMachine }
makeLenses ''UpkeepMachineRow'

type UpkeepMachineDB = UM.UpkeepMachineGen' DBText DBInt DBBool DBText DBInt
type UpkeepMachinesTable = UpkeepMachineRow' UpkeepPK MachinePK UpkeepMachineDB
type UpkeepMachinesLeftJoinTable = UpkeepMachineRow'
  (U.UpkeepId' MBInt)
  (M.MachineId' MBInt)
  (UM.UpkeepMachineGen' MBText MBInt MBBool MBText MBInt)
type UpkeepMachineRow = UpkeepMachineRow' U.UpkeepId M.MachineId UM.UpkeepMachine

makeAdaptorAndInstance' ''UM.UpkeepMachineGen'
makeAdaptorAndInstance' ''UpkeepMachineRow'

instance QueryRunnerColumnDefault PGInt4 UM.UpkeepType where
  queryRunnerColumnDefault = 
    queryRunnerColumn id UM.upkeepTypeDecode fieldQueryRunnerColumn

upkeepMachinesTable :: Table UpkeepMachinesTable UpkeepMachinesTable
upkeepMachinesTable = Table "upkeep_machines" $ pUpkeepMachineRow UpkeepMachineRow {
  _upkeepFK = pUpkeepId (U.UpkeepId . required $ "upkeep_id") ,
  _machineFK = pMachineId (M.MachineId . required $ "machine_id" ) ,
  _upkeepMachine = pUpkeepMachine UM.UpkeepMachine {
    UM.upkeepMachineNote = required "note" ,
    UM.recordedMileage = required "recorded_mileage" ,
    UM.warrantyUpkeep = required "warranty" ,
    UM.endNote = required "end_note" ,
    UM.upkeepType = required "upkeep_type" }}
