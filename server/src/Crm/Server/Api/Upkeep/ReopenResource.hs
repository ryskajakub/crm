{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- todo make it an action under upkeep

module Crm.Server.Api.Upkeep.ReopenResource ( 
  resource ) where

import           Opaleye                     (runUpdate, pgInt4, pgBool, (.===))

import           Data.Pool                   (withResource)

import           Rest.Resource               (Resource, Void, schema, name, create, mkResourceId)
import qualified Rest.Schema                 as S
import           Rest.Dictionary.Combinators (jsonI)
import           Rest.Handler                (Handler)

import           Control.Monad.Reader        (ask)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Lens                (over, set)

import qualified Crm.Shared.Upkeep           as U

import           Crm.Server.Types
import           Crm.Server.DB
import           Crm.Server.Boilerplate      ()
import           Crm.Server.Handler          (mkInputHandler')


resource :: Resource (IdDependencies' U.UpkeepId) (IdDependencies' U.UpkeepId) Void Void Void
resource = mkResourceId {
  name = "reopen" ,
  schema = S.noListing $ S.named [] ,
  create = Just reopen }

reopen :: Handler (IdDependencies' U.UpkeepId)
reopen = mkInputHandler' jsonI $ \(_ :: Double) -> do 
  ((_, pool), upkeepId) <- ask
  let
    reopenTable = set (upkeep . U.upkeepClosedL) (pgBool False) . over upkeepPK (fmap Just)
    condition = ((fmap pgInt4 upkeepId) .===) . _upkeepPK
  _ <- withResource pool $ \conn -> liftIO $ runUpdate conn upkeepsTable reopenTable condition
  return ()
