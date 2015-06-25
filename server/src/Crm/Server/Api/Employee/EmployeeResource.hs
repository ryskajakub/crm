module Crm.Server.Api.Employee.UpkeepResource ( 
  photoResource ) where

import           Opaleye.RunQuery            (runQuery)
import           Opaleye.Manipulation        (runInsert)
import           Opaleye.PGTypes             (pgInt4)

import           Rest.Resource               (Resource, Void, schema, name, create, mkResourceId, list)
import qualified Rest.Schema                 as S
import           Rest.Dictionary.Combinators (fileI, jsonO)
import           Rest.Handler                (Handler, ListHandler)

import           Control.Monad.Reader        (ask)
import           Control.Monad.IO.Class      (liftIO)

import qualified Crm.Shared.Api              as A
import qualified Crm.Shared.Photo            as P
import qualified Crm.Shared.Machine          as M
import qualified Crm.Shared.Employee         as E

import           Crm.Server.Types
import           Crm.Server.DB
import           Crm.Server.Boilerplate      ()
import           Crm.Server.Handler          (mkInputHandler', mkListing')


photoResource :: Resource (IdDependencies' E.EmployeeId) (IdDependencies' E.EmployeeId) E.EmployeeId () Void
photoResource = mkResourceId {
  name = A.photos ,
  schema = S.withListing () $ S.named [] ,
  list = const listPhotoHandler }

listPhotoHandler :: ListHandler (IdDependencies' E.EmployeeId)
listPhotoHandler = mkListing' jsonO $ const $ do
  ((_, conn), employeeId) <- ask
  rows <- liftIO $ runQuery conn (machinePhotosByMachineId employeeId)
  return $ (convert rows :: [PhotoMetaMapped])
