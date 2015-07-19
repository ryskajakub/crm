{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Crm.Server.Api.CompanyResource where

import           Opaleye.RunQuery            (runQuery)
import           Opaleye                     (pgDouble, pgStrictText, PGFloat8, Nullable, Column, runDelete, pgInt4, (.==))
import           Opaleye.Manipulation        (runInsertReturning, runUpdate)

import           Control.Monad.Trans.Class   (lift)
import           Control.Monad.Trans.Except  (ExceptT, mapExceptT)
import           Control.Monad.Reader        (ask)
import           Control.Monad.IO.Class      (liftIO, MonadIO)
import           Control.Monad               (forM, forM_)

import           Data.List                   (sortBy)
import           Data.Tuple.All              (sel1, sel2, sel3)
import qualified Data.Text.ICU               as I
import qualified Data.Map                    as M
import           Data.Pool                   (withResource)

import           Rest.Resource               (Resource, Void, schema, list, name, create, 
                                             mkResourceReaderWith, get, update, remove)
import qualified Rest.Schema                 as S
import           Rest.Dictionary.Combinators (jsonO, jsonI)
import           Rest.Handler                (ListHandler, Handler)
import           Rest.Types.Error            (Reason(..))

import           Safe                        (readMay)
import           TupleTH                     (updateAtN, proj, takeTuple)

import qualified Crm.Shared.Company          as C
import qualified Crm.Shared.Direction        as DIR
import qualified Crm.Shared.YearMonthDay     as YMD
import qualified Crm.Shared.Api              as A
import           Crm.Shared.MyMaybe

import           Crm.Server.Helpers          (prepareReaderTuple, createDeletion, 
                                             createDeletion', maybeToNullable)
import           Crm.Server.Boilerplate      ()
import           Crm.Server.Types
import           Crm.Server.DB
import           Crm.Server.Handler          (mkConstHandler', mkInputHandler', mkOrderedListing', 
                                             mkListing', updateRows'')
import           Crm.Server.CachedCore       (addNextDates, getCacheContent, recomputeSingle, recomputeWhole)


data MachineMid = NextServiceListing | MapListing

createCompanyHandler :: Handler Dependencies
createCompanyHandler = mkInputHandler' (jsonO . jsonI) $ \(newCompany, coordinates') -> do
  let coordinates = toMaybe coordinates'
  (cache, pool) <- ask
  ids <- withResource pool $ \connection -> liftIO $ runInsertReturning 
    connection 
    companiesTable
    (C.CompanyTable
      (C.CompanyId Nothing)
      (C.Company 
        (pgStrictText . C.companyName $ newCompany) 
        (pgStrictText . C.companyPlant $ newCompany)
        (pgStrictText . C.companyAddress $ newCompany))
      (C.Coordinates
        (maybeToNullable $ ((pgDouble . C.latitude) `fmap` coordinates))
        (maybeToNullable $ ((pgDouble . C.longitude) `fmap` coordinates))))
    (C.getCompanyId . C._companyPK)
  id' <- singleRowOrColumn ids
  let companyId = C.CompanyId id'
  mapExceptT lift $ withResource pool $ \connection -> recomputeSingle companyId connection cache
  return companyId

mapListing :: ListHandler Dependencies
mapListing = mkListing' jsonO (const $ unsortedResult)

unsortedResult :: ExceptT (Reason a) Dependencies 
                  [(C.CompanyId, C.Company, MyMaybe YMD.YearMonthDay, MyMaybe C.Coordinates)]
unsortedResult = do 
  (cache, _) <- ask
  content <- liftIO $ getCacheContent cache
  let asList = map (\(a,(b,c,d)) -> (a,b, toMyMaybe c, toMyMaybe d)) . M.toList
  return . asList $ content

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
      orderingByDirection $ I.compare [I.CompareIgnoreCase] (C.companyName $ sel2 r1) (C.companyName $ sel2 r2)
    Just C.NextService ->
      case (sel3 r1, sel3 r2) of
        (MyJust (date1'), MyJust(date2')) -> orderingByDirection $ date1' `compare` date2'
        -- when the date is missing, the results will always go to the bottom
        (MyNothing,MyNothing) -> EQ
        (MyNothing,_) -> GT
        _ -> LT
      ) unsortedResult')

singleCompany :: Handler (IdDependencies' C.CompanyId)
singleCompany = mkConstHandler' jsonO $ do
  ((_, pool), companyId) <- ask
  let theId = C.getCompanyId companyId
  companies <- withResource pool $ \connection -> liftIO $ runQuery connection (companyByIdCompanyQuery companyId)
  (company :: C.Company) <- singleRowOrColumn companies
  machines <- withResource pool $ \connection -> liftIO $ runMachinesInCompanyQuery theId connection
  let machinesMyMaybe = fmap ($(updateAtN 7 6) toMyMaybe . $(updateAtN 7 5) toMyMaybe) machines
  nextServiceDates <- withResource pool $ \connection -> liftIO $ forM machinesMyMaybe $ 
    \machine -> addNextDates $(proj 7 0) $(proj 7 1) machine connection
  cpRows <- withResource pool $ \connection -> liftIO $ runQuery connection $ contactPersonsByIdQuery theId
  return (
    company ,
    map (\cp -> ($(proj 3 0) cp, $(proj 3 2) cp)) . map (\x -> convert x :: ContactPersonMapped) $ cpRows ,
    machinesMyMaybe `zip` fmap $(proj 2 1) nextServiceDates )
    

updateCompany :: Handler (IdDependencies' C.CompanyId)
updateCompany = let
  readToWrite (company, coordinates') = let 
    coordinates = toMaybe coordinates'
    in \companyRow ->
      C.CompanyTable
        (Just `fmap` C._companyPK companyRow)
        (C.Company 
          (pgStrictText . C.companyName $ company) 
          (pgStrictText . C.companyPlant $ company)
          (pgStrictText . C.companyAddress $ company))
        (C.Coordinates
          (maybeToNullable $ ((pgDouble . C.latitude) `fmap` coordinates))
          (maybeToNullable $ ((pgDouble . C.longitude) `fmap` coordinates)))
  recomputeCache cId connection cache = 
    mapExceptT lift $ recomputeSingle cId connection cache
  condition companyId companyRow = C._companyPK companyRow .== fmap pgInt4 companyId
  in mkInputHandler' (jsonO . jsonI) $ \companyData -> do
    ((cache, pool), companyId) <- ask
    _ <- liftIO $ withResource pool $ \connection -> runUpdate
      connection
      companiesTable
      (readToWrite companyData)
      (condition companyId)
    withResource pool $ \connection -> recomputeCache companyId connection cache


deleteCompany :: Handler (IdDependencies' C.CompanyId)
deleteCompany = mkConstHandler' jsonO $ do
  ((cache, pool), companyId) <- ask
  let theId = C.getCompanyId companyId
  liftIO $ withResource pool $ \connection -> 
    forM_ [
      createDeletion' sel2 contactPersonsTable, const . const $
      runDelete
        connection
        companiesTable
        (\row -> C._companyPK row .== fmap pgInt4 companyId) >> return ()] $ \f -> f theId connection
  recomputeWhole pool cache

companyResource :: Resource Dependencies (IdDependencies' C.CompanyId) C.CompanyId MachineMid Void
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
    (A.single, S.singleRead id) ,
    (A.map', S.listing MapListing) ] }
