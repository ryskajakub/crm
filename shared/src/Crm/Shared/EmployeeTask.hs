{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}

module Crm.Shared.EmployeeTask where

import Crm.Shared.YearMonthDay  as YMD
import Crm.Shared.UpkeepMachine as UM

#ifndef FAY
import GHC.Generics
import Data.Data
import Rest.Info                (Info(..))
#endif
import Data.Text                (Text, pack)

#ifndef FAY
instance Info EmployeeTaskId where
  describe _ = "employeeTaskId"
instance Read EmployeeTaskId where 
  readsPrec i = fmap (\(a,b) -> (EmployeeTaskId a, b)) `fmap` readsPrec i
#endif

newtype EmployeeTaskId = EmployeeTaskId { getEmployeeTaskId :: Int }
#ifdef FAY
  deriving Eq
#else
  deriving (Eq, Generic, Typeable, Data, Show)
#endif

data EmployeeTask = EmployeeTask {
  date :: YMD.YearMonthDay ,
  task :: Text }
#ifndef FAY
  deriving (Generic, Typeable, Data)
#endif

newEmployeeTask :: EmployeeTask
newEmployeeTask = EmployeeTask {
  date = YMD.new ,
  task = pack "" }
