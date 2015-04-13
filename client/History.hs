{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
module History (pushState, replaceState, currentState) where

import Data.Text (Text)
import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Types

foreign import javascript safe
  "window.history.pushState($1, $2)"
  jsPushState2 :: JSRef a -> JSString -> IO ()

foreign import javascript safe
  "window.history.pushState($1, $2, $3)"
  jsPushState3 :: JSRef a -> JSString -> JSString -> IO ()

foreign import javascript safe
  "window.history.replaceState($1, $2)"
  jsReplaceState2 :: JSRef a -> JSString -> IO ()

foreign import javascript safe
  "window.history.replaceState($1, $2, $3)"
  jsReplaceState3 :: JSRef a -> JSString -> JSString -> IO ()

foreign import javascript safe
  "window.history.state"
  jsCurrentState :: IO (JSRef a)

pushState :: ToJSRef a => a -> JSString -> Maybe Text -> IO ()
pushState s t u = do
  s' <- toJSRef s
  case u of
   Nothing ->
     jsPushState2 s' t
   Just u' -> do
     jsPushState3 s' t (toJSString u')

replaceState :: ToJSRef a => a -> JSString -> Maybe Text -> IO ()
replaceState s t u = do
  s' <- toJSRef s
  case u of
   Nothing ->
     jsReplaceState2 s' t
   Just u' -> do
     jsReplaceState3 s' t (toJSString u')

currentState :: FromJSRef a => IO (Maybe a)
currentState = do
  s <- jsCurrentState
  fromJSRef s
