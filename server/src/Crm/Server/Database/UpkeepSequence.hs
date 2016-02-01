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

module Crm.Server.Database.UpkeepSequence where

import           Opaleye                         (required, Table(..))     
import           Data.Profunctor.Product.TH      (makeAdaptorAndInstance')
import           Control.Lens                    (makeLenses)

import qualified Crm.Shared.MachineType          as MT
import qualified Crm.Shared.UpkeepSequence       as US
import           Crm.Server.Database.PrimaryKeys
import           Crm.Server.Database.Types

data UpkeepSequenceRow' machineTypeFK upkeepSequence = UpkeepSequenceRow {
  _machineTypeFK :: machineTypeFK ,
  _upkeepSequence :: upkeepSequence }
makeLenses ''UpkeepSequenceRow'
makeAdaptorAndInstance' ''UpkeepSequenceRow'

upkeepSequencesTable :: Table UpkeepSequencesTable UpkeepSequencesTable
upkeepSequencesTable = Table "upkeep_sequences" $ pUpkeepSequenceRow UpkeepSequenceRow {
  _machineTypeFK = MT.pMachineTypeId . MT.MachineTypeId . required $ "machine_type_id" ,
  _upkeepSequence = US.pUpkeepSequence US.UpkeepSequence {
    US.displayOrdering = required "display_ordering" ,
    US.label_ = required "label" ,
    US.repetition = required "repetition" ,
    US.oneTime = required "one_time" }}

type UpkeepSequencesTable = UpkeepSequenceRow' 
  MachineTypePK
  (US.UpkeepSequence' DBInt DBText DBInt DBBool)
type UpkeepSequenceRecord = UpkeepSequenceRow' 
  MT.MachineTypeId
  MT.MachineType
