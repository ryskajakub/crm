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
#endif

newtype EmployeeId = EmployeeId { getEmployeeId :: Int }
#ifdef FAY
  deriving (Eq, Show)
#else
  deriving (Generic, Typeable, Data, Show, Read)
#endif

type Employee' = (EmployeeId, Employee)

data Employee = Employee {
  name :: Text ,
  contact :: Text ,
  capabilities :: Text }
#ifndef FAY
  deriving (Generic, Typeable, Data)
#endif

newEmployee :: Employee
newEmployee = Employee (pack "") (pack "") (pack "")
