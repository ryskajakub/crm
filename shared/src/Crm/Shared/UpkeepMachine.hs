{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Crm.Shared.UpkeepMachine where

import qualified Crm.Shared.Machine         as M
import qualified Crm.Shared.ServerRender    as SR

#ifndef FAY
import           GHC.Generics
import           Data.Data
import           Control.Lens.TH            (makeLensesFor)
#endif
import           Data.Text                  (Text, pack)

data UpkeepType =
  Regular | Repair | Check
#ifndef FAY
  deriving (Generic, Typeable, Data, Show, Eq)
#endif

type UpkeepMachine' = (UpkeepMachine, M.MachineId)

type UpkeepMachineGen upkeepMachineNote endNote = UpkeepMachineGen' upkeepMachineNote Int Bool endNote UpkeepType
type UpkeepMachine = UpkeepMachineGen Text Text
type UpkeepMachineMarkup = UpkeepMachineGen [SR.Markup] [SR.Markup]

data UpkeepMachineGen' upkeepMachineNote recordedMileage warrantyUpkeep endNote upkeepType = UpkeepMachine {
  upkeepMachineNote :: upkeepMachineNote , 
  recordedMileage :: recordedMileage ,
  warrantyUpkeep :: warrantyUpkeep ,
  endNote :: endNote ,
  upkeepType :: upkeepType }
#ifndef FAY
  deriving (Generic, Typeable, Data, Show)
makeLensesFor [("upkeepType", "upkeepTypeL")] ''UpkeepMachineGen'
#endif

newUpkeepMachine :: UpkeepMachine
newUpkeepMachine = UpkeepMachine (pack "") 0 False (pack "") Regular

upkeepTypeEncode ::
  UpkeepType ->
  Int
upkeepTypeEncode Regular = 0
upkeepTypeEncode Repair = 1
upkeepTypeEncode Check = 2

upkeepTypeDecode ::
  Int ->
  UpkeepType
upkeepTypeDecode 1 = Repair
upkeepTypeDecode 2 = Check
upkeepTypeDecode _ = Regular

allUpkeepTypes :: [UpkeepType]
allUpkeepTypes = [Regular, Repair, Check]
