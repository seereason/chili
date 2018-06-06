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
  "$1.width" js_width :: ImageData -> Int
foreign import javascript unsafe
  "$1.height" js_height :: ImageData -> Int
foreign import javascript unsafe
  "$1.data" js_getImageData :: ImageData -> Uint8ClampedArray


drawImage :: JSContext2D
          -> Image
          -> Int -- ^ dx
          -> Int -- ^ dy
          -> IO ()
drawImage = js_drawImage
{-# INLINE drawImage #-}

foreign import javascript unsafe "$1.drawImage($2,$3,$4)"
  js_drawImage :: JSContext2D -> Image -> Int -> Int -> IO ()
