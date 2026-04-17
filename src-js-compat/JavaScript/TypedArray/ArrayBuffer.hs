-- GHC JS backend compatibility shim for JavaScript.TypedArray.ArrayBuffer
module JavaScript.TypedArray.ArrayBuffer
  ( ArrayBuffer(..)
  , MutableArrayBuffer(..)
  , thaw
  , freeze
  ) where

import GHC.JS.Prim (JSVal)

newtype ArrayBuffer        = ArrayBuffer        { unArrayBuffer        :: JSVal }
newtype MutableArrayBuffer = MutableArrayBuffer { unMutableArrayBuffer :: JSVal }

-- Both are backed by the same JSVal; thaw/freeze are identity-like.
thaw :: ArrayBuffer -> IO MutableArrayBuffer
thaw (ArrayBuffer v) = return (MutableArrayBuffer v)

freeze :: MutableArrayBuffer -> IO ArrayBuffer
freeze (MutableArrayBuffer v) = return (ArrayBuffer v)
