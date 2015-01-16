module Crm.Server.Api.UpkeepResource (
  upkeepResource
) where

import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Error.Class (throwError)

import Data.Tuple.All (sel1, uncurryN)

import Rest.Types.Error (Reason(NotAllowed))
import Rest.Resource (Resource, Void, schema, list, name, mkResourceReaderWith, get)
import qualified Rest.Schema as S
import Rest.Dictionary.Combinators (jsonO, someO)
import Rest.Handler (ListHandler, mkListing, Handler, mkConstHandler)

import qualified Crm.Shared.Api as A
import qualified Crm.Shared.Company as C
import qualified Crm.Shared.Upkeep as U
import Crm.Shared.MyMaybe

import Crm.Server.Helpers (prepareReaderTuple, maybeId, readMay', dayToYmd, mapUpkeeps)
import Crm.Server.Boilerplate ()
import Crm.Server.Types
import Crm.Server.DB

data UpkeepsListing = UpkeepsAll | UpkeepsPlanned

upkeepResource :: Resource Dependencies IdDependencies UrlId UpkeepsListing Void
upkeepResource = (mkResourceReaderWith prepareReaderTuple) {
  list = \listingType -> case listingType of
    UpkeepsAll -> upkeepListing
    UpkeepsPlanned -> upkeepsPlannedListing ,
  name = A.upkeep ,
  schema = upkeepSchema ,
  get = Just upkeepCompanyMachines }

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
