{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
#if __GHCJS__
{-# LANGUAGE JavaScriptFFI #-}
#endif
module Chili.Image


-- * Image

newtype Image     = Image     { unImage :: JSVal }     deriving (Eq, Ord, Show, Read)
newtype ImageData = ImageData { unImageData :: JSVal } deriving (Eq, Ord, Show, Read)

instance IsJSVal Image
instance IsJSVal ImageData

height :: ImageData -> Int
height i = js_height i
{-# INLINE height #-}

width :: ImageData -> Int
width i = js_width i
{-# INLINE width #-}

getData :: ImageData -> Uint8ClampedArray
getData i = js_getImageData i
{-# INLINE getData #-}

foreign import javascript unsafe
  "((a1) => { return a1.width; })" js_width :: ImageData -> Int
foreign import javascript unsafe
  "((a1) => { return a1.height; })" js_height :: ImageData -> Int
foreign import javascript unsafe
  "((a1) => { return a1.data; })" js_getImageData :: ImageData -> Uint8ClampedArray


drawImage :: JSContext2D
          -> Image
          -> Int -- ^ dx
          -> Int -- ^ dy
          -> IO ()
drawImage = js_drawImage
{-# INLINE drawImage #-}

foreign import javascript unsafe "((a1,a2,a3,a4) => a1.drawImage(a2,a3,a4))"
  js_drawImage :: JSContext2D -> Image -> Int -> Int -> IO ()
