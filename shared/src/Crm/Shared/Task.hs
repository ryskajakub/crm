{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}

module Crm.Shared.Task where

import Crm.Shared.YearMonthDay  as YMD
import Crm.Shared.ServerRender  as SR

#ifndef FAY
import GHC.Generics
import Data.Data
import Rest.Info                (Info(..))
#endif
import Data.Text                (Text, pack)

#ifndef FAY
instance Info TaskId where
  describe _ = "taskId"
instance Read TaskId where 
  readsPrec i = fmap (\(a,b) -> (TaskId a, b)) `fmap` readsPrec i
#endif

newtype TaskId = TaskId { getTaskId :: Int }
#ifdef FAY
  deriving Eq
#else
  deriving (Eq, Generic, Typeable, Data, Show)
#endif

data Task' description = Task {
  startDate :: YMD.YearMonthDay ,
  description :: description ,
  endDate :: Maybe YMD.YearMonthDay }
#ifndef FAY
  deriving (Generic, Typeable, Data)
#endif
type Task = Task' Text
type TaskMarkup = Task' [SR.Markup]

newTask :: Task
newTask = Task {
  startDate = YMD.new ,
  endDate = Nothing ,
  description = pack "" }
