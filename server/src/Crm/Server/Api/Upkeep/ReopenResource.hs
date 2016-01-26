{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- todo make it an action under upkeep

module Crm.Server.Api.Upkeep.ReopenResource ( 
  resource ) where

import           Opaleye                     (runUpdate, runInsert, pgInt4, (.==), pgBool, (.===))

import           Data.Pool                   (withResource)
import           Data.ByteString.Lazy        (fromStrict, toStrict, ByteString)

import           Rest.Resource               (Resource, Void, schema, name, create, mkResourceId, list)
import qualified Rest.Schema                 as S
import           Rest.Dictionary.Combinators (fileI, jsonO, jsonI)
import           Rest.Handler                (Handler, ListHandler)

import           Control.Monad.Reader        (ask)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Lens                (over, set)

import qualified Crm.Shared.Api              as A
import qualified Crm.Shared.Photo            as P
import qualified Crm.Shared.Upkeep           as U

import           Crm.Server.Types
import           Crm.Server.DB
import           Crm.Server.Boilerplate      ()
import           Crm.Server.Handler          (mkInputHandler', mkListing')

import           TupleTH                     (updateAtN, proj, takeTuple)


resource :: Resource (IdDependencies' U.UpkeepId) (IdDependencies' U.UpkeepId) Void Void Void
resource = mkResourceId {
  name = "reopen" ,
  schema = S.noListing $ S.named [] ,
  create = Just reopen }

reopen :: Handler (IdDependencies' U.UpkeepId)
reopen = mkInputHandler' jsonI $ \(str :: Double) -> do 
  ((_, pool), upkeepId) <- ask
  let
    reopenTable = set (upkeep . U.upkeepClosedL) (pgBool False) . over upkeepPK (fmap Just)
    condition = ((fmap pgInt4 upkeepId) .===) . _upkeepPK
  _ <- withResource pool $ \conn -> liftIO $ runUpdate conn upkeepsTable reopenTable condition
  return ()
