module Chili.Canvas.ImageData where

import Chili.Canvas.Types (JSContext2D(..))
import GHCJS.Types (IsJSVal(..), JSVal)

-- * ImageData

newtype Uint8ClampedArray = Uint8ClampedArray { unUint8ClampedArray :: JSVal }
newtype ImageData = ImageData { unImageData :: JSVal }

instance IsJSVal Uint8ClampedArray
instance IsJSVal ImageData

height :: ImageData -> Int
height i = js_height i
{-# INLINE height #-}

width :: ImageData -> Int
width i = js_width i
{-# INLINE width #-}

getData :: ImageData -> Uint8ClampedArray
getData i = js_getData i
{-# INLINE getData #-}

foreign import javascript unsafe
  "$1.width" js_width :: ImageData -> Int
foreign import javascript unsafe
  "$1.height" js_height :: ImageData -> Int
foreign import javascript unsafe
  "$1.data" js_getData :: ImageData -> Uint8ClampedArray

putImageData :: JSContext2D
             -> ImageData
             -> Int
             -> Int
             -> IO ()
putImageData = js_putImageData
{-# INLINE putImageData #-}

foreign import javascript unsafe "$1.putImageData($2,$3,$4)"
  js_putImageData :: JSContext2D -> ImageData -> Int -> Int -> IO ()

getImageData :: JSContext2D
             -> Int -- ^ sx
             -> Int -- ^ sy
             -> Int -- ^ sw
             -> Int -- ^ sh
             -> IO ImageData
getImageData = js_getImageData
{-# INLINE getImageData #-}

foreign import javascript unsafe "$1.getImageData($2,$3,$4,$5)"
  js_getImageData :: JSContext2D -> Int -> Int -> Int -> Int -> IO ImageData

-- each pixel is represented by four one-byte values (red, green, blue, and alpha, in that order; that is, "RGBA" format).
newImageData :: Maybe Uint8ClampedArray
             -> Int -- width
             -> Int -- height
             -> IO ImageData
newImageData Nothing width height = js_newImageDataBlank width height
newImageData (Just d) width height = js_newImageData d width height

foreign import javascript unsafe "new ImageData($1, $2)"
        js_newImageDataBlank :: Int -> Int -> IO ImageData

foreign import javascript unsafe "new ImageData($1, $2, $3)"
        js_newImageData :: Uint8ClampedArray -> Int -> Int -> IO ImageData

