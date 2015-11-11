module Crm.Server.Base where

import           Rest.Api (Api, mkVersion, Some1(Some1), Router, route, root, compose)

import           Crm.Server.Types
import           Crm.Server.Api.CompanyResource (companyResource)
import           Crm.Server.Api.MachineResource (machineResource)
import qualified Crm.Server.Api.UpkeepResource as UR
import           Crm.Server.Api.MachineTypeResource (machineTypeResource)
import           Crm.Server.Api.EmployeeResource (employeeResource)
import qualified Crm.Server.Api.ContactPersonResource as CPR
import qualified Crm.Server.Api.Company.MachineResource as CMR
import qualified Crm.Server.Api.Company.UpkeepResource as CUP
import qualified Crm.Server.Api.Company.ContactPersonResource as CCPR
import qualified Crm.Server.Api.Machine.PhotoResource as MPR
import qualified Crm.Server.Api.PhotoMetaResource as PMR
import qualified Crm.Server.Api.MachineKindResource as MKR
import           Crm.Server.Api.PhotoResource (photoResource)
import           Crm.Server.Api.Employee.UpkeepResource as EUR
import           Crm.Server.Api.PrintResource as P
import           Crm.Server.Api.Employee.TaskResource as ETR
import           Crm.Server.Api.TaskResource as TR


router' :: Router Dependencies Dependencies
router' = root `compose` (((route companyResource `compose` route CMR.machineResource)
                                                  `compose` route CUP.upkeepResource)
                                                  `compose` route CCPR.contactPersonResource)
               `compose` (route machineResource `compose` route MPR.photoResource)
               `compose` route UR.upkeepResource
               `compose` route machineTypeResource
               `compose` ((route employeeResource `compose` route EUR.resource)
                                                  `compose` route ETR.resource)
               `compose` route PMR.photoMetaResource
               `compose` route photoResource
               `compose` route CPR.resource
               `compose` route MKR.resource
               `compose` route P.resource
               `compose` route TR.resource

api :: Api Dependencies
api = [(mkVersion 1 0 0, Some1 router')]
