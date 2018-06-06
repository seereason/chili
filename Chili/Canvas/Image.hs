module Chili.Canvas.Image where

import Chili.Canvas.Types (JSContext2D(..))
import GHCJS.Types (IsJSVal(..), JSVal)
import JavaScript.TypedArray

-- * Image

newtype Image    = Image      { unImage :: JSVal }
instance IsJSVal Image

foreign import javascript unsafe "$1[\"width\"]" width ::
         Image -> Int

foreign import javascript unsafe "$1[\"height\"]" height ::
         Image -> Int

drawImage :: JSContext2D
          -> Image
          -> Int -- ^ dx
          -> Int -- ^ dy
          -> IO ()
drawImage = js_drawImage
{-# INLINE drawImage #-}

foreign import javascript unsafe "$1.drawImage($2,$3,$4)"
  js_drawImage :: JSContext2D -> Image -> Int -> Int -> IO ()
