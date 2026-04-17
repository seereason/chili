-- GHC JS backend compatibility shim for GHCJS.Buffer
-- ByteString <-> ArrayBuffer conversions are stubbed; WebSocket/XHR
-- callers are the only users and can be updated separately.
module GHCJS.Buffer
  ( Buffer
  , fromByteString
  , getArrayBuffer
  , toByteString
  , createFromArrayBuffer
  , thaw
  , freeze
  ) where

import qualified Data.ByteString as BS
import GHC.JS.Prim (JSVal, jsNull)
import JavaScript.TypedArray.ArrayBuffer (ArrayBuffer(..), MutableArrayBuffer(..))

newtype Buffer = Buffer JSVal

fromByteString :: BS.ByteString -> (Buffer, Int, Int)
fromByteString _ = error "GHCJS.Buffer.fromByteString: not implemented for GHC JS backend"

getArrayBuffer :: Buffer -> ArrayBuffer
getArrayBuffer (Buffer v) = ArrayBuffer v

toByteString :: Int -> Maybe Int -> Buffer -> BS.ByteString
toByteString _ _ _ = error "GHCJS.Buffer.toByteString: not implemented for GHC JS backend"

createFromArrayBuffer :: ArrayBuffer -> Buffer
createFromArrayBuffer (ArrayBuffer v) = Buffer v

thaw :: ArrayBuffer -> IO MutableArrayBuffer
thaw (ArrayBuffer v) = return (MutableArrayBuffer v)

freeze :: MutableArrayBuffer -> IO ArrayBuffer
freeze (MutableArrayBuffer v) = return (ArrayBuffer v)
