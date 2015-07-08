{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Crm.Server.Api.ContactPersonResource (resource) where

import           Opaleye                     (runQuery, pgStrictText)

import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Reader        (ask)
import           Control.Applicative         ((<*>), pure)
import           Control.Lens                (_2, _3, over)

import           Data.Tuple.All              (sel1, sel2, sel3)
import           Data.Pool                   (withResource)

import           Rest.Resource               (Resource, Void, schema, name, mkResourceReaderWith, 
                                             get, update, remove)
import qualified Rest.Schema                 as S
import           Rest.Handler                (Handler)
import           Rest.Dictionary.Combinators (jsonO)

import qualified Crm.Shared.Api              as A
import qualified Crm.Shared.ContactPerson    as CP
import qualified Crm.Shared.Company          as C

import           Crm.Server.Boilerplate      ()
import           Crm.Server.Types
import           Crm.Server.DB
import           Crm.Server.Helpers          (prepareReaderTuple, createDeletion)
import           Crm.Server.Handler          (mkConstHandler', deleteRows'', updateRows'')

import           TupleTH                     (proj)

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
  let mapCoordinates coordinates = pure C.Coordinates <*> C.latitude coordinates <*> C.longitude coordinates
  (cp, company :: (C.CompanyId, C.Company, Maybe C.Coordinates)) <- over (_2._3) mapCoordinates `fmap` singleRowOrColumn rows
  return (sel3 (convert cp :: ContactPersonMapped), $(proj 3 0) company)

updateHandler :: Handler (IdDependencies' CP.ContactPersonId)
updateHandler = let
  readToWrite contactPerson row = (Nothing, sel2 row, pgStrictText $ CP.name contactPerson ,
    pgStrictText $ CP.phone contactPerson, pgStrictText $ CP.position contactPerson)
  in updateRows'' contactPersonsTable readToWrite CP.getContactPersonId (const . const . const . return $ ())
