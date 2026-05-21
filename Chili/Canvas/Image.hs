{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
#if __GHCJS__
{-# LANGUAGE JavaScriptFFI #-}
#endif
module Chili.Canvas.Image where

import Chili.Canvas.Types (JSContext2D(..))
import GHCJS.Types (IsJSVal(..), JSVal)
import JavaScript.TypedArray

-- * Image

newtype Image    = Image      { unImage :: JSVal }
instance IsJSVal Image

foreign import javascript unsafe "((a1) => a1[\"width\"])" width ::
         Image -> Int

foreign import javascript unsafe "((a1) => a1[\"height\"])" height ::
         Image -> Int

drawImage :: JSContext2D
          -> Image
          -> Int -- ^ dx
          -> Int -- ^ dy
          -> IO ()
drawImage = js_drawImage
{-# INLINE drawImage #-}

foreign import javascript unsafe "((a1,a2,a3,a4) => a1.drawImage(a2,a3,a4))"
  js_drawImage :: JSContext2D -> Image -> Int -> Int -> IO ()
