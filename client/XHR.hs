{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE RecursiveDo #-}
module XHR where

import           Control.Applicative ((<$>))
import           Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import           Control.Monad ((<=<))
import           Data.Aeson (FromJSON, decodeStrict)
import           Data.ByteString (ByteString)
import           Data.Foldable (for_)
import           Data.String
import qualified Data.Text as T
import           GHCJS.Foreign
import           GHCJS.Marshal
import           GHCJS.Types

data XMLHttpRequest
  
foreign import javascript unsafe
  "new XMLHttpRequest()"
  newXhr :: IO (JSRef XMLHttpRequest)

foreign import javascript safe
  "$1.open($2, $3, $4)"
  xhrOpen :: JSRef XMLHttpRequest -> JSString -> JSString -> JSBool -> IO ()

foreign import javascript safe
  "$1.send()"
  xhrSend :: JSRef XMLHttpRequest -> IO ()

foreign import javascript safe
  "$1.send($2)"
  xhrSendWithData :: JSRef XMLHttpRequest -> JSString -> IO ()

foreign import javascript unsafe
  "$1.onreadystatechange = $2;"
  xhrSetOnReadyStateChange :: JSRef XMLHttpRequest -> JSFun (IO ()) -> IO ()

foreign import javascript unsafe
  "$1.readyState"
  xhrGetReadyState :: JSRef XMLHttpRequest -> IO (JSRef Int)

foreign import javascript unsafe
  "$1.responseText"
  xhrGetResponseText :: JSRef XMLHttpRequest -> IO (JSString)

foreign import javascript unsafe
  "$1.response"
  xhrGetResponse :: JSRef XMLHttpRequest -> IO (JSRef a)

foreign import javascript unsafe
  "$1.responseType = $2;"
  xhrSetResponseType :: JSRef XMLHttpRequest -> JSString -> IO ()

foreign import javascript safe
  "$1.setRequestHeader($2, $3)"
  xhrSetRequestHeader :: JSRef XMLHttpRequest -> JSString -> JSString -> IO ()

foreign import javascript safe
  "document.body.innerHTML = '';"
  clearBody :: IO ()

mkRequest :: String
             -> (JSRef XMLHttpRequest -> IO ())
             -> String
             -> [(String, String)]
             -> (JSString -> IO a)
             -> IO (JSRef XMLHttpRequest)
mkRequest = mkRequestGeneric Nothing (xhrGetResponseText)

mkGet :: String -> [(String, String)] -> (JSString -> IO a) -> IO (JSRef XMLHttpRequest)
mkGet = mkRequest "GET" xhrSend

mkPost :: String -> [(String, String)] -> JSString -> (JSString -> IO a) -> IO (JSRef XMLHttpRequest)
mkPost url header d = mkRequest "POST" (flip xhrSendWithData d) url header

mkBinaryRequest :: String
                   -> (JSRef XMLHttpRequest -> IO ())
                   -> String
                   -> [(String, String)]
                   -> (ByteString -> IO a)
                   -> IO (JSRef XMLHttpRequest)
mkBinaryRequest = mkRequestGeneric (Just "arraybuffer") (bufferByteString 0 0 <=< xhrGetResponse)

mkBinaryGet :: String
               -> [(String, String)]
               -> (ByteString -> IO a)
               -> IO (JSRef XMLHttpRequest)
mkBinaryGet = mkBinaryRequest "GET" xhrSend

mkRequestGeneric :: Maybe String
                    -> (JSRef XMLHttpRequest -> IO r)
                    -> String
                    -> (JSRef XMLHttpRequest -> IO ())
                    -> String
                    -> [(String, String)]
                    -> (r -> IO a)
                    -> IO (JSRef XMLHttpRequest)
mkRequestGeneric responseType convertResponse method send url header cb = do
  xhr <- newXhr
  xhrOpen xhr (fromString method) (fromString url) jsTrue
  maybe (return ()) (xhrSetResponseType xhr . toJSString) responseType
  for_ header $ \(k, v) -> xhrSetRequestHeader xhr (toJSString k) (toJSString v)
  rec callback <- syncCallback AlwaysRetain True $ do
        readyState <- fromJSRef =<< xhrGetReadyState xhr
        if readyState == Just 4
           then do
             _ <- cb =<< convertResponse xhr
             release callback --TODO: We're assuming that the request will only be called with readyState == 4 once; is this valid?
           else return ()
  xhrSetOnReadyStateChange xhr callback
  _ <- send xhr
  return xhr

fetchJSON :: FromJSON a => T.Text -> IO a
fetchJSON url = do
  res <- newEmptyMVar
  _ <- mkBinaryGet (T.unpack url) [("Accept", "application/json")] (putMVar res)
  Just v <- decodeStrict <$> takeMVar res
  return v

postEmptyJSON :: FromJSON a => T.Text -> IO a
postEmptyJSON url = do
  res <- newEmptyMVar
  _ <- mkBinaryRequest
       "POST"
       xhrSend
       (T.unpack url)
       [("Accept", "application/json")]
       (putMVar res)
  Just v <- decodeStrict <$> takeMVar res
  return v

postJSON :: (FromJSON a, ToJSString b) => T.Text -> b -> IO a
postJSON url d = do
  res <- newEmptyMVar
  _ <- mkBinaryRequest
       "POST"
       (flip xhrSendWithData (toJSString d))
       (T.unpack url)
       [("Accept", "application/json")]
       (putMVar res)
  Just v <- decodeStrict <$> takeMVar res
  return v

foreign import javascript unsafe
  "window.location.pathname"
  pathname :: IO JSString
