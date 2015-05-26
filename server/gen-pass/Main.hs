module Main(main) where

import Opaleye (runInsert, pgStrictByteString)

import System.Console.Haskeline as H

import Control.Monad.IO.Class (liftIO)
import qualified Crypto.Scrypt as CS

import Crm.Server.DB (passwordTable, withConnection)

import Data.Text.Encoding (encodeUtf8)
import Data.Text (pack)

main :: IO ()
main = H.runInputT H.defaultSettings $ do
  password' <- H.getPassword Nothing "Password: "
  reply <- case password' of
    Just password -> do
      let pass = CS.Pass . encodeUtf8 . pack $ password
      H.outputStrLn "Using UTF-8 encoding"
      CS.EncryptedPass byteStringPassword <- liftIO $ CS.encryptPassIO CS.defaultParams pass
      _ <- liftIO $ withConnection $ \connection -> 
        runInsert connection passwordTable $ pgStrictByteString byteStringPassword
      return "password saved into db"
    Nothing -> return "reading error"
  H.outputStrLn reply
