module Crm.Runtime (
  items ,
  apiRoot ,
  withPassword ,
  passwordAjax ,
  post ,
  put ,
  delete ,
  get ) where

import           FFI                       (ffi, Automatic, Defined(Defined))
import           Data.Text                 (Text, (<>), pack, unpack)
import           Data.LocalStorage
import           Data.Defined              (fromDefined, toDefined)

import qualified JQuery                    as JQ

import           Crm.Helpers               (encodeB64)
import           Crm.Router                (CrmRouter)
import qualified Crm.Router                as R


data Items


-- methods used

post :: Text
post = pack "POST"

put :: Text
put = pack "PUT"

delete :: Text
delete = pack "DELETE"

get :: Text
get = pack "GET"


-- | Unwrap outermost layer of the fetched list in order to get to the data
items :: Items -> Automatic a
items = ffi " %1['items'] "

status :: JQ.JQXHR -> Int
status = ffi " %1['status'] "

count1000' :: String
count1000' = "count=1000"

count1000 :: String
count1000 = "?" ++ count1000'

apiRoot :: Text
apiRoot = pack "/api/v1.0.0/"

notAuthorized :: Int
notAuthorized = 401

withPassword :: Maybe Text
             -> (JQ.AjaxSettings a b -> Fay ())
             -> Fay ()
withPassword maybePassword callback = do
  password' <- case maybePassword of
    Just password -> return $ Defined password
    Nothing -> getLocalStorage $ pack "password"
  let 
    password = case fromDefined password' of
      Just passInLocalStorage -> passInLocalStorage
      Nothing -> let
        failingPassword = "none"
        in pack failingPassword 
    passwordSettings = JQ.defaultAjaxSettings {
      JQ.headers = Defined (JQ.makeRqObj (pack "Authorization") (encodeB64 password)) }
  callback passwordSettings

passwordAjax :: Text
             -> (Automatic a -> Fay ())
             -> Maybe b
             -> Text
             -> (Maybe (JQ.JQXHR -> Maybe Text -> Maybe Text -> Fay ()))
             -> Maybe Text
             -> CrmRouter
             -> Fay ()
passwordAjax url callback' inputData method' onError maybePassword router = 
  withPassword maybePassword $ \passwordSettings -> let
    commonSettings = passwordSettings {
      JQ.success = Defined callback' ,
      JQ.error' = Defined $ \xhr t1 t2 -> if status xhr == notAuthorized 
        then R.navigate R.login router
        else case onError of
          Just onError' -> onError' xhr t1 t2
          Nothing       -> return () ,
      JQ.type' = Defined method' ,
      JQ.url = Defined $ apiRoot <> url }
    in case inputData of
      Nothing -> let
        in JQ.ajax' commonSettings
      Just (data') -> let
        inputSettings = commonSettings {
          JQ.data' = Defined data' ,
          JQ.processData = Defined False ,
          JQ.contentType = Defined $ pack "application/json" ,
          JQ.dataType = Defined $ pack "json" }
        in JQ.ajax' inputSettings
