{-# LANGUAGE ScopedTypeVariables #-}

module Crm.Server.Api.Company.ContactPersonResource ( 
  contactPersonResource ) where

import           Opaleye.PGTypes             (pgInt4, pgStrictText)
import           Opaleye.Manipulation        (runInsert)
import           Opaleye.RunQuery            (runQuery)

import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Reader        (ask)

import           Data.Tuple.All              (sel1, sel3)

import           Rest.Resource               (Resource, Void, schema, name, 
                                             create, mkResourceId, list)
import qualified Rest.Schema                 as S
import           Rest.Dictionary.Combinators (jsonO, jsonI)
import           Rest.Handler                (Handler, ListHandler)

import qualified Crm.Shared.ContactPerson    as CP
import qualified Crm.Shared.Company          as C
import qualified Crm.Shared.Api              as A

import           Crm.Server.Boilerplate      ()
import           Crm.Server.Types
import           Crm.Server.DB
import           Crm.Server.Handler          (mkInputHandler', mkListing')

createContactPersonHandler :: Handler (IdDependencies' C.CompanyId)
createContactPersonHandler = mkInputHandler' (jsonO . jsonI) $ \contactPerson -> do
  ((_, connection), companyId) <- ask
  _ <- liftIO $ runInsert
    connection
    contactPersonsTable
    (Nothing, pgInt4 $ C.getCompanyId companyId, pgStrictText $ CP.name contactPerson,
      pgStrictText $ CP.phone contactPerson, pgStrictText $ CP.position contactPerson)
  return ()

contactPersonResource :: Resource (IdDependencies' C.CompanyId) (IdDependencies' C.CompanyId) Void () Void
contactPersonResource = mkResourceId {
  name = A.contactPersons ,
  schema = S.withListing () $ S.named [] ,
  list = const listing ,
  create = Just createContactPersonHandler }

listing :: ListHandler (IdDependencies' C.CompanyId)
listing = mkListing' jsonO $ const $ do
  ((_, connection), companyId) <- ask
  rawRows <- liftIO $ runQuery connection (contactPersonsByIdQuery $ C.getCompanyId companyId)
  let rowsMapped = (\x -> (sel1 x, sel3 x)) `fmap` (convert rawRows :: [ContactPersonMapped])
  return rowsMapped
