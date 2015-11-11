module Crm.Data.EmployeeData where

import Crm.Shared.Employee
import Crm.Shared.EmployeeTask

data EmployeeData = EmployeeData {
  employee :: Employee ,
  employeePageMode :: Maybe EmployeeId }

data EmployeeTasksData = EmployeeTasksData {
  employeeId :: EmployeeId ,
  employee' :: Employee ,
  employeeTasks :: [(EmployeeTaskId, EmployeeTask)] }

data EmployeeTaskData = 
  NewEmployeeTask {
    newEmployeeTask :: EmployeeTask } |
  EditEmployeeTask {
    employeeTask :: EmployeeTask } 
