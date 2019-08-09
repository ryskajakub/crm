{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import System.Process
import System.Environment
import Network.Wai
import Network.HTTP.Types.Status
import Network.HTTP.Types.Method
import qualified Data.ByteString as B
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = run 8888 app
   
app :: Application
app request respond = let 

  method = requestMethod request
  rawStdMethod = parseMethod method
  in case rawStdMethod of
    Right stdMethod ->
      if stdMethod == POST then do

        args <- getArgs
        let fileName = head args
        passwordFromFile <- B.readFile fileName
        body <- requestBody request

        if body == passwordFromFile then do
          system "sudo systemctl restart postgresql@9.5-main.service && sudo systemctl restart crm-server"
          respond $ responseBuilder status200 [] ("OK, restarting.")
        else respond $ responseBuilder status401 [] ("Wrong password")
      else respond $ responseBuilder status400 [] ("use only POST")
    Left error -> respond $ responseBuilder status400 [] ("couldn't parse method")
