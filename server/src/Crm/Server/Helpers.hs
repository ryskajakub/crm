{-# OPTIONS -fno-warn-orphans #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

module Crm.Server.Helpers (
  createDeletion ,
  createDeletion' ,
  prepareUpdate ,
  deleteRows' ,
  updateRows ,
  today ,
  ymdToDay ,
  dayToYmd ,
  maybeId ,
  withConnId ,
  readMay' ,
  maybeToNullable ,
  mapResultsToList ,
  prepareReader , 
  prepareReaderIdentity ,
  prepareReaderTuple ) where


import Control.Monad                         (forM_)
import Data.Functor.Identity                 (runIdentity)
import Data.Typeable                         (Typeable)

import           Control.Monad.Reader        (ReaderT, ask, runReaderT, mapReaderT, MonadReader)
import           Control.Monad.Trans.Class   (lift)
import           Control.Monad.Trans.Except  (ExceptT)
import           Control.Monad.IO.Class      (liftIO, MonadIO)
import           Control.Monad.Error.Class   (throwError)
import           Data.JSON.Schema.Types      (JSONSchema)
import           Data.Time.Calendar          (fromGregorian, Day, toGregorian)
import           Data.Time.Clock             (utctDay, UTCTime, getCurrentTime)
import           Data.Tuple.All              (sel1, Sel1)
import           Data.Aeson.Types            (FromJSON)
import           Database.PostgreSQL.Simple  (Connection)
import           Opaleye.Column              (Column, toNullable, Nullable)
import qualified Opaleye.Column              as COL
import           Opaleye.Manipulation        (runDelete, runUpdate)
import           Opaleye.Operators           ((.==))
import           Opaleye.PGTypes             (pgInt4, PGInt4)
import           Opaleye.Table               (Table)
import           Rest.Types.Error            (DataError(ParseError), Reason(IdentError))
import           Rest.Dictionary.Combinators (jsonO, jsonI)
import           Rest.Handler                (Handler, mkConstHandler, mkInputHandler)
import           Safe                        (readMay)

import qualified Crm.Shared.YearMonthDay     as YMD

import           Crm.Server.Types            (IdDependencies)


updateRows :: forall record m columnsW columnsR.
              (MonadIO m, MonadReader (Connection, Either String Int) m, 
                Sel1 columnsR (Column PGInt4), JSONSchema record, FromJSON record, Typeable record)
           => Table columnsW columnsR 
           -> (record -> columnsR -> columnsW) 
           -> Handler m
updateRows table readToWrite = mkInputHandler (jsonI . jsonO) 
    (\(record :: record) -> withConnId (\conn recordId -> do
  let condition row = pgInt4 recordId .== sel1 row
  _ <- liftIO $ runUpdate conn table (readToWrite record) condition
  return ()))

prepareUpdate :: (Sel1 columnsR (Column PGInt4))
              => Table columnsW columnsR
              -> (columnsR -> columnsW)
              -> Int
              -> Connection
              -> IO ()
prepareUpdate table readToWrite theId connection = runUpdate
  connection
  table
  readToWrite
  (\row -> sel1 row .== pgInt4 theId) >> return ()

deleteRows' :: [Int -> Connection -> IO ()] -> Handler IdDependencies
deleteRows' deletions = mkConstHandler jsonO $ withConnId (\connection theId -> 
  liftIO $ forM_ deletions (\deletion -> deletion theId connection))

createDeletion' :: (read -> (Column PGInt4))
                -> Table write read
                -> Int
                -> Connection
                -> IO ()
createDeletion' select table pk connection = runDelete
  connection
  table
  (\row -> select row .== pgInt4 pk) >> return ()

createDeletion :: (Sel1 read (Column PGInt4))
               => Table write read
               -> Int
               -> Connection
               -> IO ()
createDeletion = createDeletion' sel1

today :: IO Day
today = fmap utctDay getCurrentTime

ymdToDay :: YMD.YearMonthDay -> Day
ymdToDay ymd = day where 
  YMD.YearMonthDay year month day' _  = ymd
  day = fromGregorian (toInteger year) (month + 1) day'

dayToYmd :: Day -> YMD.YearMonthDay
dayToYmd day = ymd where
  (year, month, day') = toGregorian day
  ymd = YMD.YearMonthDay (fromIntegral year) (month - 1) day' YMD.DayPrecision

prepareReaderIdentity :: ReaderT (b, c) IO a
                      -> ReaderT c (ReaderT (b, c) IO) a
prepareReaderIdentity = prepareReader (\c (b, _) -> (b, c))

prepareReader :: (c -> d -> b)
              -> ReaderT b IO a
              -> ReaderT c (ReaderT d IO) a
prepareReader constructB reader = 
  mapReaderT (\cIdentity -> let
    cc = runIdentity cIdentity
    innerReader = ask >>= (\dd -> let
      constructedB = constructB cc dd
      aa = runReaderT reader constructedB
      in lift aa)
    in innerReader) outerReader
  where
    outerReader = ask

prepareReaderTuple :: ReaderT (c, b) IO a
                   -> ReaderT b (ReaderT c IO) a
prepareReaderTuple = prepareReader (\b c -> (c, b))

maybeId :: Monad m
        => Either String Int 
        -> (Int -> ExceptT (Reason r) m a)
        -> ExceptT (Reason r) m a
maybeId maybeInt onSuccess = case maybeInt of
  Right(int) -> onSuccess int
  Left(string) -> throwError $ IdentError $ ParseError
    ("provided identificator(" ++ string ++ ") cannot be parsed into number.")

withConnId :: (MonadReader (Connection, Either String Int) m)
           => (Connection -> Int -> ExceptT (Reason r) m a)
           -> ExceptT (Reason r) m a
withConnId f = do 
  (conn, id') <- ask
  maybeId id' (f conn)

readMay' :: (Read a) => String -> Either String a
readMay' string = passStringOnNoRead $ readMay string
  where
    passStringOnNoRead (Just parsed) = Right parsed
    passStringOnNoRead _ = Left string

instance Eq YMD.YearMonthDay where
  YMD.YearMonthDay y m d _ == YMD.YearMonthDay y' m' d' _ = y == y' && m == m' && d == d'
instance Ord YMD.YearMonthDay where
  ymd1 `compare` ymd2 = let
    YMD.YearMonthDay y m d _ = ymd1
    YMD.YearMonthDay y' m' d' _ = ymd2
    comp comparison nextComparison = case comparison of
      GT -> GT
      LT -> LT
      EQ -> nextComparison
    in comp (y `compare` y') $ comp (m `compare` m') $ comp (d `compare` d') EQ

maybeToNullable :: Maybe (Column a) -> Column (Nullable a)
maybeToNullable (Just a) = toNullable a
maybeToNullable Nothing = COL.null

mapResultsToList :: Eq bId
                 => (b -> bId)
                 -> (a -> b)
                 -> (a -> c)
                 -> [a]
                 -> [(b, [c])]
mapResultsToList rowIdentification mapSingle mapMultiple rows =
  foldl (\bcElements aElement -> 
    case bcElements of
      -- first element
      [] -> [(mapSingle aElement, [mapMultiple aElement])]
      -- single is the same, so add the multiple element to the list
      (bElement, cElements) : rest | rowIdentification bElement == rowIdentification newBElement ->
        (bElement, newCElement : cElements) : rest
          where 
            newBElement = mapSingle aElement
            newCElement = mapMultiple aElement
      -- single is different, so add new single and multiple
      _ -> (mapSingle aElement, [mapMultiple aElement]) : bcElements
    ) [] rows
