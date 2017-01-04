{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Crm.Server.Api.MachineKindResource where

import           Opaleye                     (pgBool, runDelete, runInsert,
                                              runInsertReturning, (.&&), (./=))
import           Opaleye.PGTypes             (pgInt4, pgStrictText, pgString)
import           Opaleye.RunQuery            (runQuery)

import           Data.Pool                   (withResource)

import           Control.Arrow               (arr, first)
import           Control.Lens
import           Control.Monad               (forM, forM_)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Reader        (ask)

import           Rest.Dictionary.Combinators (jsonI, jsonO)
import           Rest.Handler                (Handler)
import           Rest.Resource               (Resource, Void, get, mkResourceId,
                                              name, schema, update)
import qualified Rest.Schema                 as S

import qualified Crm.Shared.MachineKind      as MK

import           Crm.Server.Boilerplate      ()
import           Crm.Server.DB
import           Crm.Server.Handler          (mkConstHandler', mkInputHandler')
import           Crm.Server.Helpers
import           Crm.Server.Types

import qualified Crm.Shared.Api              as A
import qualified Crm.Shared.ExtraField       as EF
import qualified Crm.Shared.Machine          as M

import           TupleTH

resource :: Resource Dependencies Dependencies () Void Void
resource = mkResourceId {
  schema = S.noListing (S.unnamedSingle $ const ()) ,
  name = A.machineKind ,
  get = Just getter ,
  update = Just updation }

getter :: Handler Dependencies
getter = mkConstHandler' jsonO $ do
  (_, pool) <- ask
  let
    machineKindsEnums = fst `fmap` MK.machineKinds
    kindDbReprs = (first $ arr MK.kindToDbRepr) `fmap` (machineKindsEnums `zip` machineKindsEnums)
  liftIO $ forM kindDbReprs $ \(kindDbRepr, kind) -> do
    fieldsForKind <- withResource pool $ \connection -> runQuery connection (extraFieldsPerKindQuery kindDbRepr)
    let
      convert' row = (convert row :: ExtraFieldSettingsMapped)
      fieldsMapped = convert' `fmap` fieldsForKind
    return (kind, fieldsMapped)

updation :: Handler Dependencies
updation = mkInputHandler' jsonI $ \allSettings -> do
  (_, pool) <- ask
  let
    _ = allSettings :: [(MK.MachineKindEnum, [(EF.ExtraFieldIdentification, MK.MachineKindSpecific)])]
    insertSetting (machineKindEnum, extraFields) = let
      insertField (index, (fieldId, field)) = liftIO $ let
        machineKindDbRepr = MK.kindToDbRepr machineKindEnum
        allFields = (Nothing, pgInt4 $ machineKindDbRepr, pgInt4 index, pgStrictText $ MK.name field)
        in case fieldId of
          EF.ToBeAssigned -> do
            newIds <- withResource pool $ \connection ->
              runInsertReturning connection extraFieldSettingsTable allFields (view _1)
            let newExtraFieldsSettingId = (head newIds :: Int)
            (machineIdsToAddEmptyFields :: [M.MachineId]) <- withResource pool $ \connection ->
              runQuery connection (machineIdsHavingKind machineKindDbRepr)
            forM_ machineIdsToAddEmptyFields $ \machineId ->
              withResource pool $ \connection ->
                runInsert connection extraFieldsTable (pgInt4 newExtraFieldsSettingId, pgInt4 . M.getMachineId $ machineId, pgString "")
              >> return ()
            return newExtraFieldsSettingId
          EF.Assigned assignedId -> do
            let extraFieldId = EF.getExtraFieldId assignedId
            withResource pool $ \connection -> do
              let rToW fields = set _1 (Just . view _1 $ fields) allFields
              prepareUpdate extraFieldSettingsTable rToW extraFieldId connection
            return extraFieldId
      in forM ([0..] `zip` extraFields) insertField
  keepIdsNested <- forM allSettings insertSetting
  let
    keepIds = concat keepIdsNested
    mkDeletion table locateExtraFieldId = withResource pool $ \connection ->
      liftIO $ runDelete connection table $ \field -> let
        keepFieldEquations = ((locateExtraFieldId field ./=) . pgInt4) `fmap` keepIds
        in foldl (\conditionAcc condition -> conditionAcc .&& condition) (pgBool True) keepFieldEquations
  -- delete all fields that were not in the request
  _ <- mkDeletion extraFieldsTable (view _1)
  _ <- mkDeletion extraFieldSettingsTable (view _1)
  return ()
