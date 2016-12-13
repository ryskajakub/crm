module Crm.Server.Api.MachineType.PhotoResource (
  photoResource ) where

import           Crm.Server.Api.MkPhotoResource
import qualified Crm.Shared.MachineType         as MT
import           Crm.Server.Types
import           Crm.Server.DB
import           Rest.Resource                  (Resource, Void)

photoResource ::
  Resource MachineTypeDependencies MachineTypeDependencies Void () Void
photoResource = let
  fromSid (MachineTypeByName _) = undefined
  fromSid (MachineTypeById id') = id'
  in mkPhotoResource
    (MT.getMachineTypeId . fromSid)
    (machineTypePhotosByMachineTypeId . fromSid)
    machineTypePhotosTable
