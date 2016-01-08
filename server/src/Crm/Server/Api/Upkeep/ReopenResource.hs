{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Crm.Server.Api.Upkeep.ReopenResource ( 
  resource ) where

import           Opaleye                     (runUpdate, runInsert, pgInt4, (.==), pgBool)

import           Data.Pool                   (withResource)
import           Data.ByteString.Lazy        (fromStrict, toStrict, ByteString)

import           Rest.Resource               (Resource, Void, schema, name, update, mkResourceId, list)
import qualified Rest.Schema                 as S
import           Rest.Dictionary.Combinators (fileI, jsonO, jsonI)
import           Rest.Handler                (Handler, ListHandler)

import           Control.Monad.Reader        (ask)
import           Control.Monad.IO.Class      (liftIO)

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
  update = Just reopen }

reopen :: Handler (IdDependencies' U.UpkeepId)
reopen = mkInputHandler' jsonI $ \(str :: Double) -> do 
  ((_, pool), U.UpkeepId upkeepId) <- ask
  let
    reopenTable upkeep = $(updateAtN 6 2) (const . pgBool $ False) . $(updateAtN 6 0) Just $ upkeep
    condition = ((pgInt4 upkeepId) .==) . $(proj 6 0)
  _ <- withResource pool $ \conn -> liftIO $ runUpdate conn upkeepsTable reopenTable condition
  return ()
