module Crm.Server.Api.Machine.PhotoResource ( 
  photoResource ) where

import           Crm.Server.Api.MkPhotoResource
import qualified Crm.Shared.Machine             as M
import           Crm.Server.Types
import           Crm.Server.DB
import           Rest.Resource                  (Resource, Void)

photoResource :: 
  Resource (IdDependencies' M.MachineId) (IdDependencies' M.MachineId) Void () Void
photoResource =
  mkPhotoResource 
    M.getMachineId 
    machinePhotosByMachineId
    machinePhotosTable
