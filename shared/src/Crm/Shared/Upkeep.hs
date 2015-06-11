{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}

module Crm.Shared.Upkeep where

import Crm.Shared.YearMonthDay as D
import Crm.Shared.UpkeepMachine as UM

#ifndef FAY
import GHC.Generics
import Data.Data
import Rest.Info    (Info(..))
#endif
import Data.Text    (Text, pack)

#ifndef FAY
instance Info UpkeepId where
  describe _ = "upkeepId"
#endif

newtype UpkeepId = UpkeepId { getUpkeepId :: Int }
#ifdef FAY
  deriving Eq
#else
  deriving (Eq, Generic, Typeable, Data, Show, Read)
#endif

type Upkeep'' = (UpkeepId, Upkeep)
type Upkeep' = (UpkeepId, Upkeep, [UM.UpkeepMachine'])

data Upkeep = Upkeep {
  upkeepDate :: D.YearMonthDay ,
  upkeepClosed :: Bool ,
  workHours :: Text ,
  workDescription :: Text ,
  recommendation :: Text }
#ifndef FAY
  deriving (Generic, Typeable, Data)
#endif

newUpkeep :: D.YearMonthDay -> Upkeep
newUpkeep ymd = Upkeep ymd False (pack "0") (pack "") (pack "")
