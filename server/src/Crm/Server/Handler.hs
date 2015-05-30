{-# LANGUAGE FlexibleInstances #-}

module Crm.Server.Handler where

import Database.PostgreSQL.Simple (Connection)

import Opaleye.RunQuery (runQuery)
import Opaleye (queryTable, pgDouble, pgStrictText)
import Opaleye.Manipulation (runInsertReturning)

import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad (forM)

import Data.List (sortBy)
import Data.Tuple.All (sel1, sel2, sel3)
import qualified Data.Text.ICU as I
import Data.Text (pack, Text)
import Data.Maybe (mapMaybe)

import Rest.Resource (Resource, Void, schema, list, name, create, mkResourceReaderWith, get ,
  update, remove )
import qualified Rest.Schema as S
import Rest.Dictionary.Combinators (jsonO, jsonI, mkHeader, jsonE, mkPar)
import Rest.Dictionary.Types (Header(..), empty)
import Rest.Handler 
import Rest.Types.Error (Reason(..), DataError(..), DomainReason(..), ToResponseCode, toResponseCode)
import Rest.Types.Range

import qualified Crm.Shared.Company as C
import qualified Crm.Shared.Machine as M
import qualified Crm.Shared.Direction as DIR
import qualified Crm.Shared.YearMonthDay as YMD
import qualified Crm.Shared.Api as A
import Crm.Shared.MyMaybe

import Crm.Server.Helpers (prepareReaderTuple, readMay', dayToYmd, today, deleteRows', withConnId, 
  updateRows, createDeletion, createDeletion', maybeToNullable)
import Crm.Server.Boilerplate ()
import Crm.Server.Types
import Crm.Server.DB
import Crm.Server.Core (nextServiceDate, Planned (Planned, Computed))

import Safe (minimumMay, readMay)

import TupleTH (updateAtN, proj, takeTuple, catTuples)

import Debug.Trace (trace)

import qualified Crypto.Scrypt as CS
import           Safe (headMay)
import Data.Text.Encoding (encodeUtf8)
import Rest.Types.Error (Reason(..), DomainReason(..))
import Control.Monad.Error.Class (throwError)

import qualified Codec.Binary.Base64.String as B64

data SessionId = Password { password :: Text }

class HasConnection a where
  getConnection :: a -> Connection
instance HasConnection (Connection, b) where
  getConnection = fst
instance HasConnection Connection where
  getConnection = id

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
      passwordVerified = CS.verifyPass' passwordCandidate password
      password = CS.EncryptedPass dbPassword
      passwordCandidate = CS.Pass . encodeUtf8 $ inputPassword
    Nothing -> throwPasswordError 
      "password database in inconsistent state, there is either 0 or more than 1 passwords"
    where throwPasswordError = throwError . CustomReason . DomainReason

mkConstHandler' d a = mkGenHandler' d (const a)
mkListing' d a = mkGenHandler' (mkPar range . d) (a . param)
mkInputHandler' d a = mkGenHandler' d (a . input)
mkIdHandler' d a = mkGenHandler' d (\env -> ask >>= a (input env))
mkOrderedListing' d a = mkGenHandler' (mkPar orderedRange . d) (a . param)

instance ToResponseCode String where
  toResponseCode = const 401
