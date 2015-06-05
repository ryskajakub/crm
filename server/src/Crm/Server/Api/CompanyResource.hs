{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Crm.Server.Api.CompanyResource where

import           Opaleye.RunQuery            (runQuery)
import           Opaleye                     (queryTable, pgDouble, pgStrictText)
import           Opaleye.Manipulation        (runInsertReturning)

import           Control.Monad.Trans.Except  (ExceptT)
import           Control.Monad.Reader        (ask)
import           Control.Monad.IO.Class      (liftIO, MonadIO)
import           Control.Monad               (forM)

import           Data.List                   (sortBy)
import           Data.Tuple.All              (sel1, sel2, sel3)
import qualified Data.Text.ICU               as I
import           Data.Maybe                  (mapMaybe)

import           Rest.Resource               (Resource, Void, schema, list, name, create, 
                                             mkResourceReaderWith, get, update, remove)
import qualified Rest.Schema                 as S
import           Rest.Dictionary.Combinators (jsonO, jsonI)
import           Rest.Handler                (ListHandler, Handler)
import           Rest.Types.Error            (Reason(..))

import qualified Crm.Shared.Company          as C
import qualified Crm.Shared.Direction        as DIR
import qualified Crm.Shared.YearMonthDay     as YMD
import qualified Crm.Shared.Api              as A
import           Crm.Shared.MyMaybe

import           Crm.Server.Helpers          (prepareReaderTuple, readMay',
                                             deleteRows', withConnId, updateRows, createDeletion, 
                                             createDeletion', maybeToNullable)
import           Crm.Server.Boilerplate      ()
import           Crm.Server.Types
import           Crm.Server.DB
import           Crm.Server.Core             (Planned (..))
import           Crm.Server.Handler          (mkConstHandler', mkInputHandler', mkOrderedListing', mkListing')
import           Crm.Server.CachedCore       (addNextDates)

import           Safe                        (minimumMay, readMay)

import           TupleTH                     (updateAtN, proj, takeTuple)


data MachineMid = NextServiceListing | MapListing

createCompanyHandler :: Handler Dependencies
createCompanyHandler = mkInputHandler' (jsonO . jsonI) $ \(newCompany, coordinates') -> do
  let coordinates = toMaybe coordinates'
  (_,connection) <- ask
  ids <- liftIO $ runInsertReturning 
    connection 
    companiesTable
    (Nothing, pgStrictText $ C.companyName newCompany, pgStrictText $ C.companyPlant newCompany, pgStrictText $ C.companyAddress newCompany,
      maybeToNullable $ (pgDouble . C.latitude) `fmap` coordinates, maybeToNullable $ (pgDouble . C.longitude) `fmap` coordinates)
    sel1
  id' <- singleRowOrColumn ids
  return $ C.CompanyId id'

mapListing :: ListHandler Dependencies
mapListing = mkListing' jsonO (const $ unsortedResult)

unsortedResult :: ExceptT (Reason a) Dependencies 
                  [(C.CompanyId, C.Company, MyMaybe YMD.YearMonthDay, MyMaybe C.Coordinates)]
unsortedResult = do 
  (_, conn) <- ask
  rows <- liftIO $ runQuery conn (queryTable companiesTable)
  liftIO $ forM rows $ \companyRow -> do
    let companyRecord = convert companyRow :: CompanyMapped
    machines <- runMachinesInCompanyQuery (C.getCompanyId $ sel1 companyRecord) conn
    nextDays' <- forM machines $ \machine -> do
      (computationMethod, nextServiceDay) <- addNextDates $(proj 7 0) $(proj 7 1) machine conn
      return $ case computationMethod of
        Planned -> Nothing
        Computed -> Just nextServiceDay
    let nextDays = mapMaybe id nextDays'
    return $ (sel1 companyRecord, sel2 companyRecord, toMyMaybe $ minimumMay nextDays, toMyMaybe $ $(proj 3 2) companyRecord)

listing :: ListHandler Dependencies
listing = mkOrderedListing' jsonO (\(_, rawOrder, rawDirection) -> do
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
  unsortedResult'' <- unsortedResult
  let unsortedResult' = ($(takeTuple 4 3)) `fmap` unsortedResult''
  return $ sortBy (\r1 r2 -> case order of
    Nothing -> EQ
    Just C.CompanyName ->
      orderingByDirection $ I.compare [] (C.companyName $ sel2 r1) (C.companyName $ sel2 r2)
    Just C.NextService ->
      case (sel3 r1, sel3 r2) of
        (MyJust (date1'), MyJust(date2')) -> orderingByDirection $ date1' `compare` date2'
        -- when the date is missing, the results will always go to the bottom
        (MyNothing,MyNothing) -> EQ
        (MyNothing,_) -> GT
        _ -> LT
      ) unsortedResult')


singleCompany :: Handler IdDependencies
singleCompany = mkConstHandler' jsonO $ withConnId $ \conn companyId -> do
  rows <- liftIO $ runQuery conn (companyByIdQuery companyId)
  companyRow <- singleRowOrColumn rows
  machines <- liftIO $ runMachinesInCompanyQuery companyId conn
  let machinesMyMaybe = fmap ($(updateAtN 7 6) toMyMaybe . $(updateAtN 7 5) toMyMaybe) machines
  nextServiceDates <- liftIO $ forM machinesMyMaybe $ \machine -> addNextDates $(proj 7 0) $(proj 7 1) machine conn
  return (sel2 $ (convert companyRow :: CompanyMapped), machinesMyMaybe `zip` fmap $(proj 2 1) nextServiceDates)

updateCompany :: Handler IdDependencies
updateCompany = let
  readToWrite (company, coordinates') = let 
    coordinates = toMaybe coordinates'
    in const (Nothing, pgStrictText $ C.companyName company, pgStrictText $ C.companyPlant company, pgStrictText $ C.companyAddress company, 
      maybeToNullable $ (pgDouble . C.latitude) `fmap` coordinates, maybeToNullable $ (pgDouble . C.longitude) `fmap` coordinates)
  in updateRows companiesTable readToWrite

deleteCompany :: Handler IdDependencies
deleteCompany = deleteRows' [createDeletion' sel2 contactPersonsTable, createDeletion companiesTable]

companyResource :: Resource Dependencies IdDependencies UrlId MachineMid Void
companyResource = (mkResourceReaderWith prepareReaderTuple) {
  list = \type' -> case type' of
    NextServiceListing -> listing
    MapListing -> mapListing ,
  create = Just createCompanyHandler ,
  name = A.companies ,
  get = Just singleCompany ,
  update = Just updateCompany ,
  remove = Just deleteCompany ,
  schema = S.withListing NextServiceListing $ S.named [
    (A.single, S.singleBy readMay') ,
    (A.map', S.listing MapListing) ] }
