-- GHC JS backend compatibility shim for JavaScript.Web.WebSocket
module JavaScript.Web.WebSocket
  ( WebSocket(..)
  , WebSocketRequest(..)
  , connect
  , send
  ) where

import Data.JSString (JSString(..), unpack)
import GHC.JS.Prim (JSVal)
import GHC.JS.Foreign.Callback (asyncCallback1, Callback(..))
import JavaScript.Web.MessageEvent (MessageEvent(..))

newtype WebSocket = WebSocket { unWebSocket :: JSVal }

data WebSocketRequest = WebSocketRequest
  { url       :: JSString
  , protocols :: [String]
  , onClose   :: Maybe (JSVal -> IO ())
  , onMessage :: Maybe (MessageEvent -> IO ())
  }

foreign import javascript unsafe "new WebSocket($1)"
  js_newWebSocket :: JSVal -> IO JSVal

foreign import javascript unsafe "$1[\"onmessage\"] = $2"
  js_setOnMessage :: JSVal -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "$1[\"onclose\"] = $2"
  js_setOnClose :: JSVal -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "$2[\"send\"]($1)"
  js_send :: JSVal -> JSVal -> IO ()

connect :: WebSocketRequest -> IO WebSocket
connect req = do
  ws <- js_newWebSocket (unJSString (url req))
  case onMessage req of
    Nothing      -> return ()
    Just handler -> do
      cb <- asyncCallback1 (\ev -> handler (MessageEvent ev))
      js_setOnMessage ws cb
  case onClose req of
    Nothing      -> return ()
    Just handler -> do
      cb <- asyncCallback1 handler
      js_setOnClose ws cb
  return (WebSocket ws)

send :: JSString -> WebSocket -> IO ()
send (JSString v) (WebSocket ws) = js_send v ws
