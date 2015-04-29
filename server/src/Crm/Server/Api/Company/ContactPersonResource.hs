module Crm.Server.Api.Company.ContactPersonResource ( 
  contactPersonResource ) where

import Database.PostgreSQL.Simple (Connection)

import Opaleye.PGTypes (pgString, pgInt4)
import Opaleye.Manipulation (runInsertReturning)

import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)

import Data.Tuple.All (sel1)

import Rest.Resource (Resource, Void, schema, name, create, mkResourceId)
import qualified Rest.Schema as S
import Rest.Dictionary.Combinators (jsonO, someO, jsonI, someI)
import Rest.Handler (mkInputHandler, Handler)

import qualified Crm.Shared.ContactPerson as CP
import qualified Crm.Shared.Api as A

import Crm.Server.Helpers (maybeId, ymdToDay, maybeToNullable)
import Crm.Server.Boilerplate ()
import Crm.Server.Types
import Crm.Server.DB

createContactPersonHandler :: Handler IdDependencies
createContactPersonHandler = mkInputHandler (jsonO . jsonI . someI . someO) (\(contactPerson) ->
  ask >>= \(connection, maybeInt) -> maybeId maybeInt (\companyId -> liftIO $ do
    contactPersonIds <- runInsertReturning
      connection
      contactPersonsTable
      (Nothing, pgInt4 companyId, pgString $ CP.name contactPerson,
        pgString $ CP.phone contactPerson, pgString $ CP.position contactPerson)
      sel1
    let contactPersonId = head contactPersonIds
    return (contactPersonId :: Int)))

contactPersonResource :: Resource IdDependencies IdDependencies Void Void Void
contactPersonResource = mkResourceId {
  name = A.contactPersons ,
  schema = S.noListing $ S.named [] ,
  create = Just createContactPersonHandler }
