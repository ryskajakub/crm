{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Crm.Server.Api.MachineKindResource where

import Opaleye.RunQuery (runQuery)
import Opaleye.PGTypes (pgInt4, pgString)
import Opaleye (runInsertReturning, runDelete, (./=), (.&&), pgBool)

import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forM)
import Control.Arrow (arr, first)

import Rest.Resource (Resource, Void, schema, name, mkResourceId, get, update)
import qualified Rest.Schema as S
import Rest.Dictionary.Combinators (jsonO, jsonI)
import Rest.Handler (Handler, mkConstHandler, mkInputHandler)

import qualified Crm.Shared.MachineKind as MK

import Crm.Server.Helpers 
import Crm.Server.Boilerplate ()
import Crm.Server.Types
import Crm.Server.DB

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
            newIds <- runInsertReturning connection extraFieldSettingsTable allFields ($(proj 4 0))
            return $ (head newIds :: Int)
          EF.Assigned assignedId -> do
            let extraFieldId = EF.getExtraFieldId assignedId
            prepareUpdate extraFieldSettingsTable (const $ allFields) extraFieldId connection
            return extraFieldId
      in forM ([0..] `zip` extraFields) insertField
  keepIdsNested <- forM allSettings insertSetting
  let keepIds = concat keepIdsNested

  -- delete all fields that were not in the request
  _ <- liftIO $ runDelete connection extraFieldSettingsTable $ \field -> let
    keepFieldEquations = (($(proj 4 0) field ./=) . pgInt4) `fmap` keepIds
    in foldl (\conditionAcc condition -> conditionAcc .&& condition) (pgBool True) keepFieldEquations
  return ()
