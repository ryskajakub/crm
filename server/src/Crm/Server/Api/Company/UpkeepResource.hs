module Crm.Server.Api.Company.UpkeepResource (
  upkeepResource) where

import Database.PostgreSQL.Simple (Connection)

import Opaleye.PGTypes (pgInt4)
import Opaleye.Manipulation (runInsertReturning)
import Opaleye.PGTypes (pgDay, pgBool)

import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)
import Control.Applicative (pure, (<*>))

import Data.Tuple.All (sel1, upd3)

import Rest.Resource (Resource, Void, schema, name, create, list, get, mkResourceReaderWith)
import qualified Rest.Schema as S
import Rest.Dictionary.Combinators (jsonO, someO, jsonI, someI)
import Rest.Handler (mkInputHandler, Handler, ListHandler, mkListing, mkConstHandler)

import qualified Crm.Shared.UpkeepMachine as UM
import qualified Crm.Shared.Api as A
import qualified Crm.Shared.Upkeep as U
import qualified Crm.Shared.Employee as E
import Crm.Shared.MyMaybe
import Crm.Server.Api.UpkeepResource (insertUpkeepMachines)

import Crm.Server.Helpers (maybeId, ymdToDay, dayToYmd, mapUpkeeps, prepareReaderIdentity, readMay',
  maybeToNullable )
import Crm.Server.Boilerplate ()
import Crm.Server.Types
import Crm.Server.DB

companyUpkeepsListing :: ListHandler IdDependencies
companyUpkeepsListing = mkListing (jsonO . someO) (const $
  ask >>= \(conn,id') -> maybeId id' (\id'' -> do
    rows <- liftIO $ runCompanyUpkeepsQuery id'' conn
    return $ map (\((id''',u1,u2,_),(employeeId,employeeName)) -> 
      ((id''', U.Upkeep (dayToYmd u1) u2), 
        toMyMaybe $ pure (\eId e -> (eId, E.Employee e)) <*> employeeId <*> employeeName )) rows))

getUpkeep :: Handler IdDependencies
getUpkeep = mkConstHandler (jsonO . someO) ( do
  rows <- ask >>= \(conn, upkeepId') -> maybeId upkeepId' (\upkeepId ->
    liftIO $ runSingleUpkeepQuery conn upkeepId)
  let result = mapUpkeeps rows
  singleRowOrColumn (map snd result))

addUpkeep :: Connection
          -> (U.Upkeep, [(UM.UpkeepMachine, Int)], Maybe Int)
          -> IO Int -- ^ id of the upkeep
addUpkeep connection (upkeep, upkeepMachines, employeeId) = do
  upkeepIds <- runInsertReturning
    connection
    upkeepTable (Nothing, pgDay $ ymdToDay $ U.upkeepDate upkeep, 
      pgBool $ U.upkeepClosed upkeep, maybeToNullable $ fmap pgInt4 employeeId)
    sel1
  let upkeepId = head upkeepIds
  insertUpkeepMachines connection upkeepId upkeepMachines
  return upkeepId

createUpkeepHandler :: Handler IdDependencies
createUpkeepHandler = mkInputHandler (jsonO . jsonI . someI . someO) (\newUpkeep ->
  let 
    (_,_,selectedEmployeeId) = newUpkeep
    newUpkeep' = upd3 (toMaybe selectedEmployeeId) newUpkeep
    in ask >>= \(connection, maybeInt) -> maybeId maybeInt (
      -- todo check that the machines are belonging to this company
      const $ liftIO $ addUpkeep connection newUpkeep'))

upkeepResource :: Resource IdDependencies IdDependencies UrlId () Void
upkeepResource = (mkResourceReaderWith prepareReaderIdentity) {
  name = A.upkeep ,
  schema = S.withListing () $ S.unnamedSingle readMay' ,
  list = const companyUpkeepsListing ,
  get = Just getUpkeep ,
  create = Just createUpkeepHandler }
