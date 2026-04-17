-- GHC JS backend compatibility shim for JavaScript.Web.MessageEvent
module JavaScript.Web.MessageEvent
  ( MessageEvent(..)
  , MessageEventData(..)
  , getData
  ) where

import GHC.JS.Prim (JSVal)
import JavaScript.TypedArray.ArrayBuffer (ArrayBuffer(..))

newtype MessageEvent = MessageEvent { unMessageEvent :: JSVal }

data MessageEventData
  = StringData String
  | BlobData JSVal
  | ArrayBufferData ArrayBuffer

foreign import javascript unsafe "$1[\"data\"]"
  js_getData :: JSVal -> IO JSVal

getData :: MessageEvent -> IO JSVal
getData (MessageEvent v) = js_getData v
