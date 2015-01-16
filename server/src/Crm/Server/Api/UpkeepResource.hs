module Crm.Server.Api.UpkeepResource (
  insertUpkeepMachines ,
  upkeepResource ) where

import Opaleye.PGTypes (pgInt4, pgString)
import Opaleye.Operators ((.==))
import Opaleye.Manipulation (runInsert, runInsertReturning, runUpdate, runDelete)
import Opaleye.PGTypes (pgDay, pgBool)

import Database.PostgreSQL.Simple (Connection)

import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Error.Class (throwError)
import Control.Monad (forM_)

import Data.Tuple.All (sel1, uncurryN)

import Rest.Types.Error (Reason(NotAllowed))
import Rest.Resource (Resource, Void, schema, list, name, mkResourceReaderWith, get, update)
import qualified Rest.Schema as S
import Rest.Dictionary.Combinators (jsonO, someO, jsonI, someI)
import Rest.Handler (ListHandler, mkListing, Handler, mkConstHandler, mkInputHandler)

import qualified Crm.Shared.Api as A
import qualified Crm.Shared.Company as C
import qualified Crm.Shared.Upkeep as U
import qualified Crm.Shared.UpkeepMachine as UM
import Crm.Shared.MyMaybe

import Crm.Server.Helpers (prepareReaderTuple, maybeId, readMay', dayToYmd, mapUpkeeps, ymdToDay,
  maybeToNullable )
import Crm.Server.Boilerplate ()
import Crm.Server.Types
import Crm.Server.DB

data UpkeepsListing = UpkeepsAll | UpkeepsPlanned

insertUpkeepMachines :: Connection -> Int -> [(UM.UpkeepMachine, Int)] -> IO ()
insertUpkeepMachines connection upkeepId upkeepMachines = let
  insertUpkeepMachine (upkeepMachine', upkeepMachineId) = do
    _ <- runInsert
      connection
      upkeepMachinesTable (
        pgInt4 upkeepId ,
        pgString $ UM.upkeepMachineNote upkeepMachine' ,
        pgInt4 upkeepMachineId ,
        pgInt4 $ UM.recordedMileage upkeepMachine' )
    return ()
  in forM_ upkeepMachines insertUpkeepMachine

upkeepResource :: Resource Dependencies IdDependencies UrlId UpkeepsListing Void
upkeepResource = (mkResourceReaderWith prepareReaderTuple) {
  list = \listingType -> case listingType of
    UpkeepsAll -> upkeepListing
    UpkeepsPlanned -> upkeepsPlannedListing ,
  name = A.upkeep ,
  update = Just updateUpkeepHandler ,
  schema = upkeepSchema ,
  get = Just upkeepCompanyMachines }

updateUpkeepHandler :: Handler IdDependencies
updateUpkeepHandler = mkInputHandler (jsonO . jsonI . someI . someO) (\(upkeep,machines,employeeId) -> let 
  upkeepTriple = (upkeep, machines, toMaybe employeeId)
  in ask >>= \(connection, maybeInt) -> maybeId maybeInt (\upkeepId ->
    liftIO $ updateUpkeep connection upkeepId upkeepTriple))

updateUpkeep :: Connection
             -> Int
             -> (U.Upkeep, [(UM.UpkeepMachine, Int)], Maybe Int)
             -> IO ()
updateUpkeep conn upkeepId (upkeep, upkeepMachines, employeeId) = do
  _ <- let
    condition (upkeepId',_,_,_) = upkeepId' .== pgInt4 upkeepId
    readToWrite _ =
      (Nothing, pgDay $ ymdToDay $ U.upkeepDate upkeep, pgBool $ U.upkeepClosed upkeep, 
        maybeToNullable $ fmap pgInt4 employeeId)
    in runUpdate conn upkeepTable readToWrite condition
  _ <- runDelete conn upkeepMachinesTable (\(upkeepId',_,_,_) -> upkeepId' .== pgInt4 upkeepId)
  insertUpkeepMachines conn upkeepId upkeepMachines
  return ()

upkeepListing :: ListHandler Dependencies
upkeepListing = mkListing (jsonO . someO) (const $ do
  rows <- ask >>= \conn -> liftIO $ runExpandedUpkeepsQuery conn
  return $ mapUpkeeps rows) 

upkeepsPlannedListing :: ListHandler Dependencies
upkeepsPlannedListing = mkListing (jsonO . someO) (const $ do
  rows <- ask >>= \conn -> liftIO $ runPlannedUpkeepsQuery conn
  let mappedRows = map (\((uPK,u2,u3,_),companyRow) ->
        (uPK, U.Upkeep (dayToYmd u2) u3, sel1 companyRow, (uncurryN $ const C.Company) companyRow)) rows
  return mappedRows )

upkeepSchema :: S.Schema UrlId UpkeepsListing Void
upkeepSchema = S.withListing UpkeepsAll (S.named [
  (A.planned, S.listing UpkeepsPlanned) ,
  (A.single, S.singleBy readMay') ])
    
upkeepCompanyMachines :: Handler IdDependencies
upkeepCompanyMachines = mkConstHandler (jsonO . someO) (
  ask >>= \(conn, maybeUpkeepId) -> maybeId maybeUpkeepId (\upkeepId -> do
    upkeeps <- liftIO $ fmap mapUpkeeps (runSingleUpkeepQuery conn upkeepId)
    upkeep <- singleRowOrColumn upkeeps
    machines <- liftIO $ runMachinesInCompanyByUpkeepQuery upkeepId conn
    companyId <- case machines of
      [] -> throwError NotAllowed
      (companyId',_) : _ -> return companyId'
    return (companyId, (\(a,b,c) -> (a,toMyMaybe b,c)) (snd upkeep), map snd machines)))
