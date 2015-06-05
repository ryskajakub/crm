module Crm.Server.Api.ContactPersonResource (resource) where

import           Opaleye                     (runQuery, pgStrictText)

import           Control.Monad.IO.Class      (liftIO)

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
import           Crm.Server.Helpers          (prepareReaderTuple, withConnId, readMay', createDeletion)
import           Crm.Server.Handler          (mkConstHandler', deleteRows', updateRows)


resource :: Resource Dependencies IdDependencies UrlId Void Void
resource = (mkResourceReaderWith prepareReaderTuple) {
  name = A.contactPersons ,
  schema = S.noListing $ S.unnamedSingle readMay' ,
  update = Just updateHandler ,
  remove = Just deleteHandler ,
  get = Just getHandler }

deleteHandler :: Handler IdDependencies
deleteHandler = deleteRows' [createDeletion contactPersonsTable]

getHandler :: Handler IdDependencies
getHandler = mkConstHandler' jsonO $ withConnId $ \connection theId -> do
  rows <- liftIO $ runQuery connection (singleContactPersonQuery theId)
  (cp, company) <- singleRowOrColumn rows
  return $ (sel3 $ (convert cp :: ContactPersonMapped), sel1 $ (convert company :: CompanyMapped))

updateHandler :: Handler IdDependencies
updateHandler = let
  readToWrite contactPerson row = (Nothing, sel2 row, pgStrictText $ CP.name contactPerson ,
    pgStrictText $ CP.phone contactPerson, pgStrictText $ CP.position contactPerson)
  in updateRows contactPersonsTable readToWrite
