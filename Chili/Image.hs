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

#if __GHCJS__
foreign import javascript unsafe
  "$1.width" js_width :: ImageData -> Int
#elif defined(javascript_HOST_ARCH)
foreign import javascript unsafe
  "((a1) => { return a1.width; })" js_width :: ImageData -> Int
#endif
#if __GHCJS__
foreign import javascript unsafe
  "$1.height" js_height :: ImageData -> Int
#elif defined(javascript_HOST_ARCH)
foreign import javascript unsafe
  "((a1) => { return a1.height; })" js_height :: ImageData -> Int
#endif
#if __GHCJS__
foreign import javascript unsafe "$1.getImageData($2,$3,$4,$5)"
  js_getImageData :: JSContext2D -> Int -> Int -> Int -> Int -> IO ImageData
#elif defined(javascript_HOST_ARCH)
foreign import javascript unsafe
  "((a1) => { return a1.data; })" js_getImageData :: ImageData -> Uint8ClampedArray
#endif


drawImage :: JSContext2D
          -> Image
          -> Int -- ^ dx
          -> Int -- ^ dy
          -> IO ()
drawImage = js_drawImage
{-# INLINE drawImage #-}

#if __GHCJS__
foreign import javascript unsafe "$1.drawImage($2,$3,$4)"
  js_drawImage :: JSContext2D -> Image -> Int -> Int -> IO ()
#elif defined(javascript_HOST_ARCH)
foreign import javascript unsafe "((a1,a2,a3,a4) => a1.drawImage(a2,a3,a4))"
  js_drawImage :: JSContext2D -> Image -> Int -> Int -> IO ()
#endif
