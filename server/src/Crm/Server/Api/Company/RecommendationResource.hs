{-# LANGUAGE TemplateHaskell #-}

module Crm.Server.Api.Company.RecommendationResource ( 
  resource ) where

import           Control.Monad               (forM_)

import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Trans.Except  (ExceptT)
import           Control.Monad.Reader        (ask)
import           Data.Tuple.All              (sel1)
import           Data.Text                   (Text)
import           Data.Pool                   (withResource)
import           Database.PostgreSQL.Simple  (Connection)
import           Opaleye.PGTypes             (pgInt4, pgStrictText, pgDay, pgBool)
import           Opaleye.Manipulation        (runInsert, runInsertReturning)
import           Opaleye                     (runQuery)
import           Rest.Resource               (Resource, Void, schema, name, get,
                                             create, mkResourceId, list)
import qualified Rest.Schema                 as S
import           Rest.Dictionary.Combinators (jsonO)
import           Rest.Handler                (Handler)
import           Rest.Types.Error            (Reason)

import qualified Crm.Shared.UpkeepSequence   as US
import qualified Crm.Shared.MachineType      as MT
import qualified Crm.Shared.MachineKind      as MK
import qualified Crm.Shared.Machine          as M
import qualified Crm.Shared.ContactPerson    as CP
import qualified Crm.Shared.Company          as C
import qualified Crm.Shared.ExtraField       as EF
import qualified Crm.Shared.Api              as A
import           Crm.Shared.MyMaybe          (toMaybe)

import           Crm.Server.Helpers          (ymdToDay, maybeToNullable)
import           Crm.Server.Boilerplate      ()
import           Crm.Server.Types
import           Crm.Server.DB
import           Crm.Server.Handler          (mkConstHandler')
import           Crm.Server.CachedCore       (recomputeSingle)

import           TupleTH                     (proj)


getter :: Handler (IdDependencies' C.CompanyId)
getter = mkConstHandler' jsonO $ do
  ((_, pool), companyId) <- ask
  lastUpkeeps <- liftIO $ withResource pool $ \connection -> runQuery connection (lastRecommendationQuery companyId)
  lastUpkeep <- singleRowOrColumn lastUpkeeps
  let lastUpkeepMapped = convert lastUpkeep :: UpkeepMapped
  return lastUpkeepMapped

resource :: Resource (IdDependencies' C.CompanyId) (IdDependencies' C.CompanyId) () Void Void
resource = mkResourceId {
  name = "recommendation" ,
  schema = S.singleton () $ S.named [] ,
  get = Just getter }
