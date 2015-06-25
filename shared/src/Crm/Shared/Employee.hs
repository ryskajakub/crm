{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}

module Crm.Shared.Employee where

#ifndef FAY
import GHC.Generics
import Data.Data
import Rest.Info    (Info(..))
#endif
import Data.Text    (Text, pack)

#ifndef FAY
instance Info EmployeeId where
  describe _ = "employeeId"
instance Read EmployeeId where 
  readsPrec i = fmap (\(a,b) -> (EmployeeId a, b)) `fmap` readsPrec i
#endif

newtype EmployeeId = EmployeeId { getEmployeeId :: Int }
#ifdef FAY
  deriving (Eq, Show)
#else
  deriving (Generic, Typeable, Data, Show, Eq)
#endif

type Employee' = (EmployeeId, Employee)

data Employee = Employee {
  name :: Text ,
  contact :: Text ,
  capabilities :: Text }
#ifdef FAY
  deriving (Eq)
#else
  deriving (Generic, Typeable, Data, Eq)
#endif

newEmployee :: Employee
newEmployee = Employee (pack "") (pack "") (pack "")
