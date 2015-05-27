{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Crm.Server.Api.LoginResource where

import qualified Crypto.Scrypt as CS

import Opaleye (queryTable, runQuery)

import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Error.Class (throwError)

import Rest.Types.Error (Reason(..), DomainReason(..), ToResponseCode, toResponseCode)
import Rest.Resource (Resource, Void, schema, name, mkResourceId, statics)
import qualified Rest.Schema as S
import Rest.Dictionary.Combinators (jsonE, jsonI)
import Rest.Handler (Handler)

import Crm.Server.Helpers 
import Crm.Server.Boilerplate ()
import Crm.Server.Types
import Crm.Server.DB
import Crm.Server.Handler (mkInputHandler')

import Data.Text.Encoding (encodeUtf8)

import qualified Crm.Shared.Login as L
import qualified Crm.Shared.Api as A

import           Safe (headMay)

resource :: Resource Dependencies Dependencies Void Void ()
resource = mkResourceId {
  schema = S.noListing $ S.named [(A.perform, S.static ())] ,
  statics = const login ,
  name = A.login } 

login :: Handler Dependencies
login = mkInputHandler' (jsonE . jsonI) $ \login -> do
  connection <- ask
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
      passwordCandidate = CS.Pass . encodeUtf8 . L.password $ login
    Nothing -> throwPasswordError 
      "password database in inconsistent state, there is either 0 or more than 1 passwords"
    where throwPasswordError = throwError . CustomReason . DomainReason

instance ToResponseCode String where
  toResponseCode = const 401
