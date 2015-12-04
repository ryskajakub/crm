module Crm.Data.EmployeeData where

import Data.Text                (Text)

import Crm.Shared.Employee
import Crm.Shared.Task

import Crm.Component.DatePicker

data EmployeeData = EmployeeData {
  employee :: Employee ,
  employeePageMode :: Maybe EmployeeId ,
  takenColours :: [Text] }

data EmployeeTasksData = EmployeeTasksData {
  employeeId :: EmployeeId ,
  openTasks :: [(TaskId, Task)] ,
  closedTasks :: [(TaskId, Task)] }

data EmployeeTaskPageMode =
  Close TaskId |
  Edit TaskId |
  New EmployeeId

data EmployeeTaskData = EmployeeTaskData {
  employeeTask :: Task ,
  taskDatePicker :: DatePickerData ,
  identification :: EmployeeTaskPageMode }
