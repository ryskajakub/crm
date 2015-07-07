{-# OPTIONS -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Crm.Server.Handler where

import           Control.Monad               (forM_)

import qualified Codec.Binary.Base64.String  as B64
import           Control.Monad.Error.Class   (throwError)
import           Control.Monad.Trans.Except  (ExceptT, withExceptT)
import           Control.Monad.Reader        (ask, ReaderT)
import           Control.Monad.IO.Class      (liftIO, MonadIO)
import           Control.Monad.Reader.Class  (MonadReader)
import qualified Crypto.Scrypt               as CS
import           Data.Aeson.Types            (FromJSON)
import           Data.JSON.Schema.Types      (JSONSchema)
import           Data.Text                   (pack, Text)
import           Data.Text.Encoding          (encodeUtf8)
import           Data.Time.Calendar          (fromGregorian, Day)
import           Data.Tuple.All              (sel1, Sel1, uncurryN)
import           Database.PostgreSQL.Simple  (Connection)
import           Opaleye.RunQuery            (runQuery)
import           Opaleye                     (queryTable, pgInt4, PGInt4, Table, Column, (.==), runUpdate)
import           Rest.Dictionary.Combinators (mkHeader, jsonE, mkPar, jsonO, jsonI)
import           Rest.Dictionary.Types       (Header(..), Modifier, FromMaybe, Dict, Param(..))
import           Rest.Handler                hiding (mkConstHandler, mkInputHandler, mkListing, mkOrderedListing, mkIdHandler)
import           Rest.Types.Error            (Reason(..), DataError(..), DomainReason(..), 
                                             ToResponseCode, toResponseCode)
import           Rest.Types.Void             (Void) 
import           Safe                        (headMay)
import           Data.Typeable               (Typeable)

import           Crm.Server.ListParser       (parseDate)
import           Crm.Server.Boilerplate      ()
import           Crm.Server.DB
import           Crm.Server.Types
import           Crm.Server.Helpers

import           TupleTH                     (reverseTuple, updateAtN)


data SessionId = Password { password :: Text }

class HasConnection a where
  getConnection :: a -> Connection
instance HasConnection (Connection, b) where
  getConnection = fst
instance HasConnection GlobalBindings where
  getConnection = snd
instance HasConnection ((c, Connection), b) where
  getConnection = snd . fst
instance HasConnection Connection where
  getConnection = id

mkGenHandler' :: 
  (MonadReader a m, MonadIO m, HasConnection a) => 
  Modifier () p i o Nothing -> 
  (Env SessionId p (FromMaybe () i) -> ExceptT (Reason Text) m (Apply f (FromMaybe () o))) -> 
  GenHandler m f
mkGenHandler' d a = mkGenHandler (jsonE . authorizationHeader . d) $ \env -> do
  connection <- ask
  verifyPassword (getConnection connection) (header env)
  a env
  where
  authorizationHeader = mkHeader $ Header ["Authorization"] $ \headers' -> case headers' of
    [Just authHeader] -> Right . Password . pack . B64.decode $ authHeader
    _ -> Left . ParseError $ "data not parsed correctly"

verifyPassword :: (Monad m, MonadIO m) => Connection -> SessionId -> ExceptT (Reason Text) m ()
verifyPassword connection (Password inputPassword) = do
  dbPasswords <- liftIO $ runQuery connection $ queryTable passwordTable
  let dbPassword' = headMay dbPasswords
  case dbPassword' of
    Just dbPassword -> 
      if passwordVerified
        then return ()
        else throwPasswordError "wrong password"
      where
      passwordVerified = CS.verifyPass' passwordCandidate password'
      password' = CS.EncryptedPass dbPassword
      passwordCandidate = CS.Pass . encodeUtf8 $ inputPassword
    Nothing -> throwPasswordError 
      "password database in inconsistent state, there is either 0 or more than 1 passwords"
    where throwPasswordError = throwError . CustomReason . DomainReason . pack

mkConstHandler' :: (MonadReader a m, MonadIO m, HasConnection a) 
                => Modifier () p Nothing o Nothing
                -> ExceptT (Reason Text) m (FromMaybe () o)
                -> Handler m
mkConstHandler' d a = mkGenHandler' d (const a)

mkInputHandler' :: (MonadReader a m, MonadIO m, HasConnection a)
                => Modifier () p (Just i) o Nothing
                -> (i -> ExceptT (Reason Text) m (FromMaybe () o))
                -> Handler m
mkInputHandler' d a = mkGenHandler' d (a . input)

mkIdHandler' :: (MonadReader a m, MonadIO m, HasConnection a)
             => Modifier () p (Just i) o Nothing
             -> (i -> a -> ExceptT (Reason Text) m (FromMaybe () o))
             -> Handler m
mkIdHandler' d a = mkGenHandler' d (\env -> ask >>= a (input env))

mkListing' :: (MonadReader a m, MonadIO m, HasConnection a)
           => Modifier () () Nothing o Nothing
           -> (Range -> ExceptT (Reason Text) m [FromMaybe () o])
           -> ListHandler m
mkListing' d a = mkGenHandler' (mkPar range . d) (a . param)

mkOrderedListing' :: (MonadReader a m, MonadIO m, HasConnection a)
                  => Modifier () () Nothing o Nothing
                  -> ((Range, Maybe String, Maybe String) -> ExceptT (Reason Text) m [FromMaybe () o])
                  -> ListHandler m
mkOrderedListing' d a = mkGenHandler' (mkPar orderedRange . d) (a . param)

instance ToResponseCode Text where
  toResponseCode = const 401

updateRows' :: forall record m columnsW columnsR.
               (Functor m, MonadIO m, MonadReader (GlobalBindings, Either String Int) m, 
                 Sel1 columnsR (Column PGInt4), JSONSchema record, FromJSON record, Typeable record)
            => Table columnsW columnsR 
            -> (record -> columnsR -> columnsW) 
            -> (Int -> Connection -> Cache -> ExceptT (Reason Void) m ())
            -> Handler m
updateRows' table readToWrite postUpdate = mkInputHandler' (jsonI . jsonO) $ \(record :: record) -> let
  doUpdation = withConnId' $ \conn cache recordId -> do
    let condition row = pgInt4 recordId .== sel1 row
    _ <- liftIO $ runUpdate conn table (readToWrite record) condition
    postUpdate recordId conn cache
  in withExceptT (const . CustomReason . DomainReason . pack $ "updation failed") doUpdation

updateRows :: forall record m columnsW columnsR.
              (Functor m, MonadIO m, MonadReader (GlobalBindings, Either String Int) m, 
                Sel1 columnsR (Column PGInt4), JSONSchema record, FromJSON record, Typeable record)
           => Table columnsW columnsR 
           -> (record -> columnsR -> columnsW) 
           -> Handler m
updateRows table readToWrite = updateRows' table readToWrite (const . const . const . return $ ())

deleteRows' :: [Int -> Connection -> IO ()] -> Handler IdDependencies
deleteRows' deletions = mkConstHandler' jsonO $ withConnId $ \connection theId ->
  liftIO $ forM_ deletions $ \deletion -> deletion theId connection

deleteRows'' :: (MonadIO m) => [Int -> Connection -> IO ()] -> Int -> Connection -> m ()
deleteRows'' deletions theId connection = 
  liftIO $ forM_ deletions $ \deletion -> deletion theId connection

updateRows'' :: forall record m columnsW columnsR recordId.
                (Functor m, MonadIO m, 
                  Sel1 columnsR (Column PGInt4), JSONSchema record, FromJSON record, Typeable record)
             => Table columnsW columnsR 
             -> (record -> columnsR -> columnsW) 
             -> (recordId -> Int)
             -> (Int -> Connection -> Cache -> ExceptT (Reason Void) (ReaderT (GlobalBindings, recordId) m) ())
             -> Handler (ReaderT (GlobalBindings, recordId) m)
updateRows'' table readToWrite showInt postUpdate = mkInputHandler' (jsonI . jsonO) $ \(record :: record) -> do
  ((cache, conn), recordId) <- ask
  let doUpdation = do
        let condition row = pgInt4 (showInt recordId) .== sel1 row
        _ <- liftIO $ runUpdate conn table (readToWrite record) condition
        postUpdate (showInt recordId) conn cache
  withExceptT (const . CustomReason . DomainReason . pack $ "updation failed") doUpdation

mkDayParam :: Dict h p i o e -> Dict h (Int, Int, Int) i o e
mkDayParam = mkPar $ Param ["day"] parse
  where
  parse [Just day] = case parseDate day of
    Left _ -> Left . ParseError $ "day parse failed"
    Right r -> Right r
  parse _ = Left . MissingField $ "day parameter not present"

getDayParam :: Env h (Int, Int, Int) i -> Day
getDayParam = uncurryN fromGregorian . $(updateAtN 3 0) fromIntegral . $(reverseTuple 3) . param
