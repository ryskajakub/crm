{-# OPTIONS -fno-warn-orphans #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

module Crm.Server.Helpers (
  parseMarkupOrPlain ,
  createDeletion ,
  createDeletion' ,
  prepareUpdate ,
  today ,
  maybeId ,
  withConnId ,
  withConnId' ,
  readMay' ,
  maybeToNullable ,
  mapResultsToList ,
  prepareReader , 
  prepareReaderIdentity ,
  prepareReaderTuple ,
  catchError ) where


import           Data.Functor.Identity       (runIdentity)
import           Data.Text                   (Text)

import           Control.Monad.Reader        (ReaderT, ask, runReaderT, mapReaderT)
import           Control.Monad.Trans.Class   (lift)
import           Control.Monad.Trans.Except  (ExceptT)
import           Control.Monad.Error.Class   (throwError)
import           Data.Time.Calendar          (Day)
import           Data.Time.Clock             (utctDay, getCurrentTime)
import           Data.Tuple.All              (sel1, Sel1)
import           Data.Pool                   (withResource)
import           Database.PostgreSQL.Simple  (Connection)
import           Opaleye.Column              (Column, toNullable, Nullable)
import qualified Opaleye.Column              as COL
import           Opaleye.Manipulation        (runDelete, runUpdate)
import           Opaleye.Operators           ((.==))
import           Opaleye.PGTypes             (pgInt4, PGInt4)
import           Opaleye.Table               (Table)
import           Rest.Types.Error            (DataError(ParseError), Reason(IdentError))
import           Safe                        (readMay)

import qualified Crm.Shared.YearMonthDay     as YMD
import qualified Crm.Shared.ServerRender     as SR

import           Crm.Server.Types            (GlobalBindings, Cache)
import           Crm.Server.Parsers          (parseMarkup)


catchError :: Either a b -> Maybe b
catchError (Right r) = Just r
catchError (Left {}) = Nothing

parseMarkupOrPlain :: 
  Text ->
  [SR.Markup]
parseMarkupOrPlain text = either 
  (const . (:[]) . SR.PlainText $ text) (id)
  . parseMarkup $ text

prepareUpdate :: 
  (Sel1 columnsR (Column PGInt4)) => 
  Table columnsW columnsR -> 
  (columnsR -> columnsW) -> 
  Int -> 
  Connection -> 
  IO ()
prepareUpdate table readToWrite theId connection = runUpdate
  connection
  table
  readToWrite
  (\row -> sel1 row .== pgInt4 theId) >> return ()

createDeletion' :: 
  (read -> (Column PGInt4)) -> 
  Table write read -> 
  Int -> 
  Connection -> 
  IO ()
createDeletion' select table pk connection = runDelete
  connection
  table
  (\row -> select row .== pgInt4 pk) >> return ()

createDeletion :: 
  (Sel1 read (Column PGInt4)) => 
  Table write read -> 
  Int -> 
  Connection -> 
  IO ()
createDeletion = createDeletion' sel1

today :: IO Day
today = fmap utctDay getCurrentTime

prepareReaderIdentity :: 
  ReaderT (b, c) IO a -> 
  ReaderT c (ReaderT (b, c) IO) a
prepareReaderIdentity = prepareReader (\c (b, _) -> (b, c))

prepareReader :: 
  (c -> d -> b) -> 
  ReaderT b IO a -> 
  ReaderT c (ReaderT d IO) a
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

prepareReaderTuple :: 
  ReaderT (c, b) IO a -> 
  ReaderT b (ReaderT c IO) a
prepareReaderTuple = prepareReader (\b c -> (c, b))

maybeId :: 
  Monad m => 
  Either String Int -> 
  (Int -> ExceptT (Reason r) m a) -> 
  ExceptT (Reason r) m a
maybeId maybeInt onSuccess = case maybeInt of
  Right(int) -> onSuccess int
  Left(string) -> throwError $ IdentError $ ParseError
    ("provided identificator(" ++ string ++ ") cannot be parsed into number.")

withConnId' :: 
  (Connection -> Cache -> Int -> ExceptT (Reason r) (ReaderT (GlobalBindings, Either String Int) IO) a) -> 
  ExceptT (Reason r) (ReaderT (GlobalBindings, Either String Int) IO) a
withConnId' f = do 
  ((cache, pool), id') <- ask
  withResource pool $ \connection -> maybeId id' (f connection cache)

withConnId :: 
  (Connection -> Int -> ExceptT (Reason r) (ReaderT (GlobalBindings, Either String Int) IO) a) -> 
  ExceptT (Reason r) (ReaderT (GlobalBindings, Either String Int) IO) a
withConnId f = withConnId' $ \connection -> const $ f connection

readMay' :: (Read a) => String -> Either String a
readMay' string = passStringOnNoRead $ readMay string
  where
    passStringOnNoRead (Just parsed) = Right parsed
    passStringOnNoRead _ = Left string

maybeToNullable :: Maybe (Column a) -> Column (Nullable a)
maybeToNullable (Just a) = toNullable a
maybeToNullable Nothing = COL.null

mapResultsToList :: 
  Eq bId => 
  (b -> bId) -> 
  (a -> b) -> 
  (a -> c) -> 
  [a] -> 
  [(b, [c])]
mapResultsToList rowIdentification mapSingle mapMultiple rows =
  reverse . foldl (\bcElements aElement -> 
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
    ) [] $ rows
