{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Crm.Server.Api.MachineKindResource where

import Opaleye.RunQuery (runQuery)
import Opaleye.PGTypes (pgInt4, pgString, pgDay)
import Opaleye (runInsert, runUpdate)

import Data.List (zip)

import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)
import Control.Applicative (liftA3)
import Control.Monad (forM_, forM)
import Control.Arrow (arr, first)

import Rest.Resource (Resource, Void, schema, list, name, mkResourceId, get, update, remove)
import qualified Rest.Schema as S
import Rest.Dictionary.Combinators (jsonO, jsonI)
import Rest.Handler (ListHandler, mkListing, Handler, mkConstHandler, mkInputHandler)

import qualified Crm.Shared.MachineKind as MK

import Crm.Server.Helpers (prepareReaderTuple, readMay', dayToYmd, today, deleteRows',
  withConnId, ymdToDay, maybeToNullable, createDeletion, prepareUpdate)
import Crm.Server.Boilerplate ()
import Crm.Server.Types
import Crm.Server.DB
import Crm.Server.Core (nextServiceDate)

import qualified Crm.Shared.ExtraField as EF
import qualified Crm.Shared.Api as A

import TupleTH
import Data.Tagged

resource :: Resource Dependencies Dependencies () Void Void
resource = mkResourceId {
  schema = S.noListing (S.unnamedSingle $ const ()) ,
  name = A.machineKind ,
  get = Just getter ,
  update = Just updation }

getter :: Handler Dependencies
getter = mkConstHandler jsonO $ do
  connection <- ask
  let 
    machineKindsEnums = fst `fmap` MK.machineKinds
    kindDbReprs = (first $ arr MK.kindToDbRepr) `fmap` (machineKindsEnums `zip` machineKindsEnums)
  liftIO $ forM kindDbReprs $ \(kindDbRepr, kind) -> do
    fieldsForKind <- runQuery connection (extraFieldsPerKindQuery kindDbRepr)
    let fieldsMapped = unTagged (convert fieldsForKind :: ExtraFieldSettingsMapped)
    return (kind, fieldsMapped)

updation :: Handler Dependencies
updation = mkInputHandler jsonI $ \allSettings -> do
  connection <- ask 
  let
    s = allSettings :: [(MK.MachineKindEnum, [(EF.ExtraFieldIdentification, MK.MachineKindSpecific)])]
    insertSetting (machineKindEnum, extraFields) = let
      insertField (index, (fieldId, field)) = liftIO $ let
        allFields = (Nothing, pgInt4 $ MK.kindToDbRepr machineKindEnum, pgInt4 index, pgString $ MK.name field)
        in case fieldId of
          EF.ToBeAssigned -> do
            runInsert connection extraFieldSettingsTable allFields
            return ()
          EF.Assigned assignedId ->
            prepareUpdate extraFieldSettingsTable (const $ allFields) (EF.getExtraFieldId assignedId) connection
      in forM_ ([0..] `zip` extraFields) insertField
  forM_ allSettings insertSetting
