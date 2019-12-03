{-# LANGUAGE ViewPatterns #-}

module Main(main) where

import           Data.Text                (pack)
import           Data.Text.Encoding       (encodeUtf8)
import           Control.Monad.IO.Class   (liftIO)
import qualified Crypto.Scrypt            as CS
import           Opaleye                  (runInsert, pgStrictByteString, runDelete, pgBool, Table)
import           System.Console.Haskeline as H
import qualified System.Exit              as Exit

import           Crm.Server.DB            (passwordTable, withConnection, readonlyPasswordTable, PasswordTable)


tableByChar :: Char -> Maybe (Table PasswordTable PasswordTable)
tableByChar 'r' = Just readonlyPasswordTable
tableByChar 'w' = Just passwordTable
tableByChar _ = Nothing


main :: IO ()
main = H.runInputT H.defaultSettings $ do
  permissionType <- getInputChar "Insert readonly permission password / write permission password (r/w)"
  case permissionType of
    Just (tableByChar -> Just table) -> do
      password' <- H.getPassword Nothing "Password: "
      reply <- case password' of
        Just password -> do
          let pass = CS.Pass . encodeUtf8 . pack $ password
          H.outputStrLn "Using UTF-8 encoding"
          CS.EncryptedPass byteStringPassword <- liftIO $ CS.encryptPassIO CS.defaultParams pass
          _ <- liftIO $ withConnection $ \connection -> do
            runInsert connection table $ pgStrictByteString byteStringPassword
          return "password saved into db"
        Nothing -> return "reading error"
      H.outputStrLn reply
    _ -> do 
      H.outputStrLn "type (r/w) next time. Exiting"
      liftIO Exit.exitFailure
