-- GHC JS backend compatibility shim for JavaScript.TypedArray.ArrayBuffer
module JavaScript.TypedArray.ArrayBuffer
  ( ArrayBuffer(..)
  , MutableArrayBuffer(..)
  , thaw
  , freeze
  ) where

import GHC.JS.Prim (JSVal)
import GHCJS.Marshal.Pure (PToJSVal(..), PFromJSVal(..))

newtype ArrayBuffer        = ArrayBuffer        { unArrayBuffer        :: JSVal }
newtype MutableArrayBuffer = MutableArrayBuffer { unMutableArrayBuffer :: JSVal }

instance PToJSVal ArrayBuffer        where pToJSVal (ArrayBuffer v)               = v
instance PFromJSVal ArrayBuffer       where pFromJSVal                             = ArrayBuffer
instance PToJSVal MutableArrayBuffer  where pToJSVal (MutableArrayBuffer v)        = v
instance PFromJSVal MutableArrayBuffer where pFromJSVal                            = MutableArrayBuffer

-- Both are backed by the same JSVal; thaw/freeze are identity-like.
thaw :: ArrayBuffer -> IO MutableArrayBuffer
thaw (ArrayBuffer v) = return (MutableArrayBuffer v)

freeze :: MutableArrayBuffer -> IO ArrayBuffer
freeze (MutableArrayBuffer v) = return (ArrayBuffer v)
