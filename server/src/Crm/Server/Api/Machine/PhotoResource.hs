module Crm.Server.Api.Machine.PhotoResource ( 
  photoResource ) where

import Rest.Resource (Resource, Void, schema, name, create, mkResourceId)
import qualified Rest.Schema as S
import Rest.Dictionary.Combinators (fileI, someI)
import Rest.Handler (mkInputHandler, Handler)

import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)

import qualified Crm.Shared.Api as A
import Crm.Server.Types
import Crm.Server.DB (addMachinePhoto)
import Crm.Server.Helpers (maybeId)

photoResource :: Resource IdDependencies IdDependencies Void Void Void
photoResource = mkResourceId {
  name = A.machines ,
  schema = S.noListing $ S.named [] ,
  create = Just addPhotoHandler }

addPhotoHandler :: Handler IdDependencies
addPhotoHandler = mkInputHandler (fileI . someI) (\photo -> do 
  (connection, maybeMachineIdInt) <- ask
  maybeId maybeMachineIdInt (\machineId ->
    liftIO $ addMachinePhoto connection machineId photo))
