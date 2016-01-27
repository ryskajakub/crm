module Crm.Server.Api.MachineType.MachineResource ( 
  resource ) where

import           Opaleye.RunQuery            (runQuery)
import           Opaleye.Manipulation        (runInsert)
import           Opaleye.PGTypes             (pgInt4)

import           Data.Pool                   (withResource)
import           Data.ByteString.Lazy        (fromStrict, toStrict, ByteString)

import           Rest.Resource               (Resource, Void, schema, name, create, mkResourceId, list)
import qualified Rest.Schema                 as S
import           Rest.Dictionary.Combinators (fileI, jsonO)
import           Rest.Handler                (Handler, ListHandler)

import           Control.Lens                (_1, _2, _Just, over, mapped, view)
import           Control.Monad.Reader        (ask)
import           Control.Monad.IO.Class      (liftIO)

import qualified Crm.Shared.Api              as A
import qualified Crm.Shared.MachineType      as MT
import qualified Crm.Shared.Machine          as M
import qualified Crm.Shared.Company          as C
import qualified Crm.Shared.Upkeep           as U

import           Crm.Server.Types
import           Crm.Server.DB
import           Crm.Server.Boilerplate      ()
import           Crm.Server.Handler          (mkInputHandler', mkListing')
import           Crm.Server.Helpers          (dayToYmd)


resource :: Resource MachineTypeDependencies MachineTypeDependencies Void () Void
resource = mkResourceId {
  name = "machines" ,
  schema = S.withListing () $ S.named [] ,
  list = const listing }

listing :: ListHandler MachineTypeDependencies
listing = mkListing' jsonO $ const $ do
  ((_, pool), machineTypeSid) <- ask
  rows' <- liftIO $ withResource pool $ \connection -> runQuery connection (machinesForTypeQ machineTypeSid)
  let rows = over (mapped . _1 . machine . M.operationStartDateL . _Just) dayToYmd
        . over (mapped . _2 . C.companyCoords) (\co -> pure C.Coordinates <*> C.latitude co <*> C.longitude co)
        $ rows'
        :: [(MachineRecord, C.CompanyRecord)]
  return $ over mapped (
      over _2 (\company' -> (view C.companyPK company', view C.companyCore company'))
    . over _1 (\machine' -> (view machinePK machine', view machine machine')))
    rows
