module Crm.Server.Api.MachineType.MachineResource ( 
  resource ) where

import           Opaleye.RunQuery            (runQuery)

import           Data.Pool                   (withResource)

import           Rest.Resource               (Resource, Void, schema, name, mkResourceId, list)
import qualified Rest.Schema                 as S
import           Rest.Dictionary.Combinators (jsonO)
import           Rest.Handler                (ListHandler)

import           Control.Lens                (_1, _2, over, mapped, view)
import           Control.Monad.Reader        (ask)
import           Control.Monad.IO.Class      (liftIO)

import qualified Crm.Shared.Company          as C

import           Crm.Server.Types
import           Crm.Server.DB
import           Crm.Server.Boilerplate      ()
import           Crm.Server.Handler          (mkListing')


resource :: Resource MachineTypeDependencies MachineTypeDependencies Void () Void
resource = mkResourceId {
  name = "machines" ,
  schema = S.withListing () $ S.named [] ,
  list = const listing }

listing :: ListHandler MachineTypeDependencies
listing = mkListing' jsonO $ const $ do
  ((_, pool), machineTypeSid) <- ask
  rows' <- liftIO $ withResource pool $ \connection -> runQuery connection (machinesForTypeQ machineTypeSid)
  let rows = over (mapped . _2 . companyCoords) (\co -> pure C.Coordinates <*> C.latitude co <*> C.longitude co) $ rows' :: [(MachineRecord, CompanyRecord)]
  return $ over mapped (
      over _2 (\company' -> (view companyPK company', view companyCore company'))
    . over _1 (\machine' -> (view machinePK machine', view machine machine')))
    rows
