{-# LANGUAGE ScopedTypeVariables #-}

module Crm.Server.Api.ContactPersonResource (resource) where

import           Opaleye                     (pgStrictText, runQuery)

import           Control.Lens                (over, view, _1, _2)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Reader        (ask)

import           Data.Pool                   (withResource)
import           Data.Tuple.All              (sel2, sel3)

import           Rest.Dictionary.Combinators (jsonO)
import           Rest.Handler                (Handler)
import           Rest.Resource               (Resource, Void, get,
                                              mkResourceReaderWith, name,
                                              remove, schema, update)
import qualified Rest.Schema                 as S

import qualified Crm.Shared.Api              as A
import qualified Crm.Shared.Company          as C
import qualified Crm.Shared.ContactPerson    as CP

import           Crm.Server.Boilerplate      ()
import           Crm.Server.DB
import           Crm.Server.Handler          (deleteRows'', mkConstHandler',
                                              updateRows'')
import           Crm.Server.Helpers          (createDeletion,
                                              prepareReaderTuple)
import           Crm.Server.Types


resource :: Resource Dependencies (IdDependencies' CP.ContactPersonId) CP.ContactPersonId Void Void
resource = (mkResourceReaderWith prepareReaderTuple) {
  name = A.contactPersons ,
  schema = S.noListing $ S.unnamedSingleRead id ,
  update = Just updateHandler ,
  remove = Just deleteHandler ,
  get = Just getHandler }

deleteHandler :: Handler (IdDependencies' CP.ContactPersonId)
deleteHandler = mkConstHandler' jsonO $ do
  ((_, pool), contactPersonId) <- ask
  deleteRows'' [createDeletion contactPersonsTable] (CP.getContactPersonId contactPersonId) pool

getHandler :: Handler (IdDependencies' CP.ContactPersonId)
getHandler = mkConstHandler' jsonO $ do
  ((_, pool), contactPersonId) <- ask
  rows <- liftIO $ withResource pool $ \connection ->
    runQuery connection (singleContactPersonQuery $ CP.getContactPersonId contactPersonId)
  (cp, company :: CompanyRecord) <- over (_2 . companyCoords) C.mapCoordinates `fmap` singleRowOrColumn rows
  return (sel3 (convert cp :: ContactPersonMapped), _companyPK company)

updateHandler :: Handler (IdDependencies' CP.ContactPersonId)
updateHandler = let
  readToWrite contactPerson row = (Just . view _1 $ row, sel2 row, pgStrictText $ CP.name contactPerson ,
    pgStrictText $ CP.phone contactPerson, pgStrictText $ CP.position contactPerson)
  in updateRows'' contactPersonsTable readToWrite CP.getContactPersonId (const . const . const . return $ ())
