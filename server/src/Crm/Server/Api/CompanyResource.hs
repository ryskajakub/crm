{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Crm.Server.Api.CompanyResource where

import Opaleye.PGTypes (pgString)
import Opaleye.RunQuery (runQuery)
import Opaleye (queryTable)

import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forM)

import Data.List (sortBy)
import Data.Tuple.All (sel1, sel2, sel3, sel6, upd6)
import qualified Data.Text.ICU as I
import Data.Text (pack)

import Rest.Resource (Resource, Void, schema, list, name, create, mkResourceReaderWith, get ,
  update, remove )
import qualified Rest.Schema as S
import Rest.Dictionary.Combinators (jsonO, jsonI)
import Rest.Handler (ListHandler, mkOrderedListing, mkInputHandler, Handler, mkConstHandler)

import qualified Crm.Shared.Company as C
import qualified Crm.Shared.Machine as M
import qualified Crm.Shared.Upkeep as U
import qualified Crm.Shared.Direction as DIR
import qualified Crm.Shared.Api as A
import Crm.Shared.MyMaybe

import Crm.Server.Helpers (prepareReaderTuple, readMay', dayToYmd, today, deleteRows', withConnId, 
  updateRows, createDeletion, maybeToNullable)
import Crm.Server.Boilerplate ()
import Crm.Server.Types
import Crm.Server.DB
import Crm.Server.Core (nextServiceDate)

import Safe (minimumMay, readMay)

createCompanyHandler :: Handler Dependencies
createCompanyHandler = mkInputHandler (jsonO . jsonI) (\newCompany ->
  ask >>= \conn -> liftIO $ addCompany conn newCompany )

listing :: ListHandler Dependencies
listing = mkOrderedListing jsonO (\(_, rawOrder, rawDirection) -> do
  let 
    order = rawOrder >>= readMay
    direction = rawDirection >>= readMay
    -- "negate" the ordering when it is descending
    orderingByDirection :: Ordering -> Ordering
    orderingByDirection = case direction of
      Nothing -> id
      Just(DIR.Asc) -> id
      Just(DIR.Desc) -> \ord -> case ord of
        LT -> GT
        GT -> LT
        EQ -> EQ
  conn <- ask 
  rows <- liftIO $ runQuery conn (queryTable companiesTable)
  unsortedResult <- liftIO $ forM rows (\companyRow -> do
    let companyRecord = convert companyRow :: CompanyMapped
    machines <- runMachinesInCompanyQuery (C.getCompanyId $ sel1 companyRecord) conn
    nextDays <- forM machines (\(machineId, machine, _, _, _, _) -> do
      upkeepRows <- runQuery conn (nextServiceUpkeepsQuery $ M.getMachineId machineId)
      upkeepSequenceRows <- runQuery conn (nextServiceUpkeepSequencesQuery $ M.getMachineId machineId)
      today' <- today
      let
        upkeeps = convert upkeepRows :: [UpkeepMapped] 
        upkeepSequences = fmap (\r -> sel2 (convert r :: UpkeepSequenceMapped)) upkeepSequenceRows
        upkeepSequenceTuple = case upkeepSequences of
          [] -> undefined
          x : xs -> (x, xs)
        nextServiceDay = nextServiceDate machine upkeepSequenceTuple (fmap sel3 upkeeps) today'
      return $ dayToYmd nextServiceDay)
    return $ (sel1 companyRecord, sel2 companyRecord, toMyMaybe $ minimumMay nextDays))
  return $ sortBy (\r1 r2 -> case order of
    Nothing -> EQ
    Just C.CompanyName ->
      orderingByDirection $ I.compare [] (pack $ C.companyName $ sel2 r1) (pack $ C.companyName $ sel2 r2)
    Just C.NextService ->
      case (sel3 r1, sel3 r2) of
        (MyJust (date1'), MyJust(date2')) -> orderingByDirection $ date1' `compare` date2'
        -- when the date is missing, the results will always go to the bottom
        (MyNothing,MyNothing) -> EQ
        (MyNothing,_) -> GT
        _ -> LT
      ) unsortedResult)

singleCompany :: Handler IdDependencies
singleCompany = mkConstHandler jsonO $ withConnId (\conn companyId -> do
  rows <- liftIO $ runQuery conn (companyByIdQuery companyId)
  companyRow <- singleRowOrColumn rows
  machines <- liftIO $ runMachinesInCompanyQuery companyId conn
  let machinesMyMaybe = fmap (\m -> upd6 (toMyMaybe $ sel6 m) m) machines
  return (sel2 $ (convert companyRow :: CompanyMapped) , machinesMyMaybe))

updateCompany :: Handler IdDependencies
updateCompany = let
  readToWrite company = const (Nothing, pgString $ C.companyName company, 
    pgString $ C.companyPlant company, pgString $ C.companyAddress company, maybeToNullable Nothing, maybeToNullable Nothing)
  in updateRows companiesTable readToWrite

deleteCompany :: Handler IdDependencies
deleteCompany = deleteRows' [createDeletion companiesTable]

companyResource :: Resource Dependencies IdDependencies UrlId () Void
companyResource = (mkResourceReaderWith prepareReaderTuple) {
  list = const listing ,
  create = Just createCompanyHandler ,
  name = A.companies ,
  get = Just singleCompany ,
  update = Just updateCompany ,
  remove = Just deleteCompany ,
  schema = S.withListing () $ S.unnamedSingle readMay' }
