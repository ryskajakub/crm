{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Crm.Server.Handler where

import           Database.PostgreSQL.Simple  (Connection)

import           Opaleye.RunQuery            (runQuery)
import           Opaleye                     (queryTable)

import qualified Codec.Binary.Base64.String  as B64

import           Control.Monad.Error.Class   (throwError)
import           Control.Monad.Trans.Except  (ExceptT)
import           Control.Monad.Reader        (ask)
import           Control.Monad.IO.Class      (liftIO, MonadIO)

import           Control.Monad.Reader.Class  (MonadReader)

import           Data.Text                   (pack, Text)
import           Data.Text.Encoding          (encodeUtf8)

import           Rest.Dictionary.Combinators (mkHeader, jsonE, mkPar, xmlE)
import           Rest.Dictionary.Types       (Header(..), Modifier, FromMaybe)
import           Rest.Handler 
import           Rest.Types.Error            (Reason(..), DataError(..), 
                                             DomainReason(..), ToResponseCode, 
                                             toResponseCode)
import           Rest.Types.Void (Void)

import           Crm.Server.Boilerplate      ()
import           Crm.Server.DB

import qualified Crypto.Scrypt               as CS
import           Safe (headMay)


data SessionId = Password { password :: Text }

class HasConnection a where
  getConnection :: a -> Connection
instance HasConnection (Connection, b) where
  getConnection = fst
instance HasConnection Connection where
  getConnection = id

mkGenHandler' :: (MonadReader a m, MonadIO m, HasConnection a) 
              => Modifier () p i o Nothing
              -> (Env SessionId p (FromMaybe () i) -> ExceptT (Reason String) m (Apply f (FromMaybe () o)))
              -> GenHandler m f
mkGenHandler' d a = mkGenHandler (jsonE . authorizationHeader . d) $ \env -> do
  connection <- ask
  verifyPassword (getConnection connection) (header env)
  a env
  where
  authorizationHeader = mkHeader $ Header ["Authorization"] $ \headers' -> case headers' of
    [Just authHeader] -> Right . Password . pack . B64.decode $ authHeader
    _ -> Left . ParseError $ "data not parsed correctly"

verifyPassword :: (Monad m, MonadIO m) => Connection -> SessionId -> ExceptT (Reason String) m ()
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
    where throwPasswordError = throwError . CustomReason . DomainReason

mkConstHandler' :: (MonadReader a m, MonadIO m, HasConnection a) 
                => Modifier () p Nothing o Nothing
                -> ExceptT (Reason String) m (FromMaybe () o)
                -> Handler m
mkConstHandler' d a = mkGenHandler' d (const a)

mkInputHandler' :: (MonadReader a m, MonadIO m, HasConnection a)
                => Modifier () p (Just i) o Nothing
                -> (i -> ExceptT (Reason String) m (FromMaybe () o))
                -> Handler m
mkInputHandler' d a = mkGenHandler' d (a . input)

mkIdHandler' :: (MonadReader a m, MonadIO m, HasConnection a)
             => Modifier () p (Just i) o Nothing
             -> (i -> a -> ExceptT (Reason String) m (FromMaybe () o))
             -> Handler m
mkIdHandler' d a = mkGenHandler' d (\env -> ask >>= a (input env))

mkListing' d a = mkGenHandler' (mkPar range . d) (a . param)
mkOrderedListing' d a = mkGenHandler' (mkPar orderedRange . d) (a . param)

instance ToResponseCode String where
  toResponseCode = const 401
