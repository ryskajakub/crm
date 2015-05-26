{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Crm.Server.Api.LoginResource where

import qualified Crypto.Scrypt as CS

import Opaleye (queryTable, runQuery)

import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)

import Rest.Resource (Resource, Void, schema, name, mkResourceId, statics)
import qualified Rest.Schema as S
import Rest.Dictionary.Combinators (jsonO, jsonI)
import Rest.Handler (Handler, mkInputHandler)

import Crm.Server.Helpers 
import Crm.Server.Boilerplate ()
import Crm.Server.Types
import Crm.Server.DB

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
login = mkInputHandler (jsonO . jsonI) $ \login -> do
  connection <- ask
  dbPasswords <- liftIO $ runQuery connection (queryTable passwordTable)
  let 
    dbPassword' = headMay dbPasswords
    passwordOK = case dbPassword' of
      Just dbPassword -> 
        CS.verifyPass' passwordCandidate password
        where
        password = CS.EncryptedPass dbPassword
        passwordCandidate = CS.Pass . encodeUtf8 . L.password $ login
      Nothing -> False
  return ""
