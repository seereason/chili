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
#elif (defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH))
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1.width; })($1)" js_width :: ImageData -> Int
#else
foreign import javascript unsafe
  "((a1) => { return a1.width; })" js_width :: ImageData -> Int
#endif
#endif
#if __GHCJS__
#elif (defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH))
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1.height; })($1)" js_height :: ImageData -> Int
#else
foreign import javascript unsafe
  "((a1) => { return a1.height; })" js_height :: ImageData -> Int
#endif
#endif
#if __GHCJS__
#elif (defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH))
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1.data; })($1)" js_getImageData :: ImageData -> Uint8ClampedArray
#else
foreign import javascript unsafe
  "((a1) => { return a1.data; })" js_getImageData :: ImageData -> Uint8ClampedArray
#endif
#endif


drawImage :: JSContext2D
          -> Image
          -> Int -- ^ dx
          -> Int -- ^ dy
          -> IO ()
drawImage = js_drawImage
{-# INLINE drawImage #-}

#if __GHCJS__
#elif (defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH))
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2,a3,a4) => a1.drawImage(a2,a3,a4))($1,$2,$3,$4)"
#else
foreign import javascript unsafe "((a1,a2,a3,a4) => a1.drawImage(a2,a3,a4))"
#endif
  js_drawImage :: JSContext2D -> Image -> Int -> Int -> IO ()
#endif
