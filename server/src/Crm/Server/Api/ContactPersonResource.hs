module Crm.Server.Api.ContactPersonResource (resource) where

import           Opaleye                     (runQuery, pgStrictText)

import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Reader        (ask)

import           Data.Tuple.All              (sel1, sel2, sel3)

import           Rest.Resource               (Resource, Void, schema, name, mkResourceReaderWith, 
                                             get, update, remove)
import qualified Rest.Schema                 as S
import           Rest.Handler                (Handler)
import           Rest.Dictionary.Combinators (jsonO)

import qualified Crm.Shared.Api              as A
import qualified Crm.Shared.ContactPerson    as CP

import           Crm.Server.Boilerplate      ()
import           Crm.Server.Types
import           Crm.Server.DB
import           Crm.Server.Helpers          (prepareReaderTuple, createDeletion)
import           Crm.Server.Handler          (mkConstHandler', deleteRows'', updateRows'')


resource :: Resource Dependencies (IdDependencies' CP.ContactPersonId) CP.ContactPersonId Void Void
resource = (mkResourceReaderWith prepareReaderTuple) {
  name = A.contactPersons ,
  schema = S.noListing $ S.unnamedSingleRead id ,
  update = Just updateHandler ,
  remove = Just deleteHandler ,
  get = Just getHandler }

deleteHandler :: Handler (IdDependencies' CP.ContactPersonId)
deleteHandler = mkConstHandler' jsonO $ do
  ((_,connection), contactPersonId) <- ask
  deleteRows'' [createDeletion contactPersonsTable] (CP.getContactPersonId contactPersonId) connection

getHandler :: Handler (IdDependencies' CP.ContactPersonId)
getHandler = mkConstHandler' jsonO $ do
  ((_, connection), contactPersonId) <- ask
  rows <- liftIO $ runQuery connection (singleContactPersonQuery $ CP.getContactPersonId contactPersonId)
  (cp, company) <- singleRowOrColumn rows
  return $ (sel3 $ (convert cp :: ContactPersonMapped), sel1 $ (convert company :: CompanyMapped))

updateHandler :: Handler (IdDependencies' CP.ContactPersonId)
updateHandler = let
  readToWrite contactPerson row = (Nothing, sel2 row, pgStrictText $ CP.name contactPerson ,
    pgStrictText $ CP.phone contactPerson, pgStrictText $ CP.position contactPerson)
  in updateRows'' contactPersonsTable readToWrite CP.getContactPersonId (const . const . const . return $ ())
