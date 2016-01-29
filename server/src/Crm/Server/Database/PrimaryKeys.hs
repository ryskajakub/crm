{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Crm.Server.Database.PrimaryKeys where

import           Data.Profunctor.Product.TH

import qualified Crm.Shared.Machine as M
import qualified Crm.Shared.Upkeep  as U
import           Crm.Server.Database.Types

type UpkeepPK  = U.UpkeepId'  DBInt
type MachinePK = M.MachineId' DBInt

makeAdaptorAndInstance' ''M.MachineId'
makeAdaptorAndInstance' ''U.UpkeepId'
