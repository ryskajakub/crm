module Crm.Data.EmployeeData where

import Data.Text                (Text)

import Crm.Shared.Employee
import Crm.Shared.Task

import Crm.Component.DatePicker

data EmployeeData = EmployeeData {
  employee :: Employee ,
  employeePageMode :: Maybe EmployeeId }

data EmployeeTasksData = EmployeeTasksData {
  employeeId :: EmployeeId ,
  openTasks :: [(TaskId, Task)] ,
  closedTasks :: [(TaskId, Task)] }

data EmployeeTaskData = EmployeeTaskData {
  employeeTask :: Task ,
  taskDatePicker :: DatePickerData ,
  identification :: Either TaskId EmployeeId }
