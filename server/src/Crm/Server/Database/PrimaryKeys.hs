{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Crm.Server.Database.PrimaryKeys where

import qualified Crm.Shared.Machine        as M
import qualified Crm.Shared.MachineType    as MT
import qualified Crm.Shared.Upkeep         as U
import qualified Crm.Shared.Company        as C
import           Crm.Server.Database.Types

type UpkeepPK      = U.UpkeepId'       DBInt
type MachinePK     = M.MachineId'      DBInt
type MachineTypePK = MT.MachineTypeId' DBInt
type CompanyPK     = C.CompanyId'      DBInt
