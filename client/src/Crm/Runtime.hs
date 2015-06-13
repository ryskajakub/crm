module Crm.Runtime (
  items ,
  count1000 ,
  apiRoot ,
  withPassword ,
  passwordAjax ,
  post ,
  put ,
  delete ,
  get ) where

import           FFI                       (ffi, Automatic, Defined(Defined))
import           Prelude                   hiding (putStrLn)
import           Data.Text                 (Text, (<>), pack)
import           Data.LocalStorage
import           Data.Defined              (fromDefined, toDefined)

import qualified JQuery                    as JQ

import           Crm.Helpers               (encodeB64)


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

count1000 :: String
count1000 = "?count=1000"

apiRoot :: Text
apiRoot = pack "/api/v1.0.0/"

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
        failingPassword = ""
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
             -> Fay ()
passwordAjax url callback' inputData method' onError maybePassword = 
  withPassword maybePassword $ \passwordSettings -> let
    commonSettings = passwordSettings {
      JQ.success = Defined callback' ,
      JQ.error' = toDefined onError ,
      JQ.type' = Defined method' ,
      JQ.url = Defined $ apiRoot <> url <> pack count1000 }
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
