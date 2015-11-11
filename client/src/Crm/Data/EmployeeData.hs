module Crm.Data.EmployeeData where

import Data.Text               (Text)

import Crm.Shared.Employee
import Crm.Shared.EmployeeTask

import Crm.Component.DatePicker

data EmployeeData = EmployeeData {
  employee :: Employee ,
  employeePageMode :: Maybe EmployeeId }

data EmployeeTasksData = EmployeeTasksData {
  employeeId :: EmployeeId ,
  employeeTasks :: [(EmployeeTaskId, EmployeeTask)] }

data EmployeeTaskData = EmployeeTaskData {
  employeeTask :: EmployeeTask ,
  taskDatePicker :: (DatePicker, Text) ,
  identification :: Either EmployeeTaskId EmployeeId }
