{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Crm.Server.Api.Company.RecommendationResource ( 
  resource ) where

import           Control.Lens                (view)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Reader        (ask)

import           Data.Pool                   (withResource)
import           Opaleye                     (runQuery)
import           Rest.Resource               (Resource, Void, schema, name, get, mkResourceId)
import qualified Rest.Schema                 as S
import           Rest.Dictionary.Combinators (jsonO)
import           Rest.Handler                (Handler)

import qualified Crm.Shared.Upkeep           as U
import qualified Crm.Shared.Company          as C
import           Crm.Shared.MyMaybe          (toMyMaybe)

import           Crm.Server.Boilerplate      ()
import           Crm.Server.Types
import           Crm.Server.DB
import           Crm.Server.Handler          (mkConstHandler')

import           Safe                        (headMay)


getter :: Handler (IdDependencies' C.CompanyId)
getter = mkConstHandler' jsonO $ do
  ((_, pool), companyId) <- ask
  lastUpkeep <- liftIO . withResource pool $ \connection -> runQuery connection (lastRecommendationQuery companyId)
  let _ = lastUpkeep :: [UpkeepRow'' U.UpkeepId U.Upkeep]
  let lastUpkeepMapped = headMay . fmap (\u -> (view upkeepPK u, view upkeep u)) $ lastUpkeep
  let _ = lastUpkeepMapped :: Maybe (U.UpkeepId, U.Upkeep)
  return . toMyMaybe $ lastUpkeepMapped

resource :: Resource (IdDependencies' C.CompanyId) (IdDependencies' C.CompanyId) () Void Void
resource = mkResourceId {
  name = "recommendation" ,
  schema = S.singleton () $ S.named [] ,
  get = Just getter }
