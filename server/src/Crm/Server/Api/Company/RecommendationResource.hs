{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Crm.Server.Api.Company.RecommendationResource (
  resource ) where

import           Control.Lens                (mapped, over, view)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Reader        (ask)

import           Data.Pool                   (withResource)
import           Opaleye                     (runQuery)
import           Rest.Dictionary.Combinators (jsonO)
import           Rest.Handler                (Handler)
import           Rest.Resource               (Resource, Void, get, mkResourceId,
                                              name, schema)
import qualified Rest.Schema                 as S

import qualified Crm.Shared.Company          as C
import           Crm.Shared.MyMaybe          (toMyMaybe)
import qualified Crm.Shared.Upkeep           as U

import           Crm.Server.Boilerplate      ()
import           Crm.Server.DB
import           Crm.Server.Handler          (mkConstHandler')
import           Crm.Server.Types

import           Safe                        (headMay)


getter :: Handler (IdDependencies' C.CompanyId)
getter = mkConstHandler' jsonO $ do
  ((_, pool), companyId) <- ask
  lastUpkeep :: [UpkeepRow] <- liftIO . withResource pool $ \connection ->
    over (mapped . mapped . upkeepSuper) (\x -> fmap (U.UpkeepId) (U.getUpkeepId x)) $
      runQuery connection (lastRecommendationQuery companyId)
  let _ = lastUpkeep :: [UpkeepRow'' U.UpkeepId U.Upkeep (Maybe U.UpkeepId)]
  let lastUpkeepMapped = headMay . fmap (\u -> (view upkeepPK u, view upkeep u)) $ lastUpkeep
  let _ = lastUpkeepMapped :: Maybe (U.UpkeepId, U.Upkeep)
  return . toMyMaybe $ lastUpkeepMapped

resource :: Resource (IdDependencies' C.CompanyId) (IdDependencies' C.CompanyId) () Void Void
resource = mkResourceId {
  name = "recommendation" ,
  schema = S.singleton () $ S.named [] ,
  get = Just getter }
