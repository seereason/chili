{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
#if __GHCJS__
{-# LANGUAGE JavaScriptFFI #-}
#endif
{-# language DataKinds #-}
{-# language DeriveDataTypeable #-}
{-# language KindSignatures #-}
{-# language TypeFamilies #-}
{-# language OverloadedStrings #-}
module Chili.PointerEventObject where

#if defined(wasm32_HOST_ARCH)
import Dominator.Types (JSElement(..))
#endif
import Chili.Types
import Control.Monad.Trans (MonadIO(liftIO))
import qualified Data.JSString as JS
import Data.JSString.Text (textToJSString, textFromJSString)
import Data.Data (Data, Typeable)
import GHCJS.Marshal (ToJSVal(..), FromJSVal(..))
import GHCJS.Marshal.Pure (PToJSVal(pToJSVal), PFromJSVal(pFromJSVal))
import GHCJS.Nullable (Nullable(..), nullableToMaybe, maybeToNullable)
import GHCJS.Types (IsJSVal(..), JSVal(..), JSString(..),  nullRef, isNull, isUndefined)


-- * PointerEvent properties (read-only)

newtype PointerId = PointerId { unPointerId :: Int }
  deriving (Eq, Ord, Read, Show, Data, Typeable)

#if __GHCJS__
#elif (defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH))
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"pointerId\"]; })($1)" pointerId ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"pointerId\"]; })" pointerId ::
#endif
        PointerEventObject ev -> PointerId
#endif

#if __GHCJS__
#elif (defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH))
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"width\"]; })($1)" width ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"width\"]; })" width ::
#endif
        PointerEventObject ev -> Double
#endif

#if __GHCJS__
#elif (defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH))
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"height\"]; })($1)" height ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"height\"]; })" height ::
#endif
        PointerEventObject ev -> Double
#endif

#if __GHCJS__
#elif (defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH))
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"pressure\"]; })($1)" pressure ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"pressure\"]; })" pressure ::
#endif
        PointerEventObject ev -> Float
#endif

#if __GHCJS__
#elif (defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH))
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"tangentialPressure\"]; })($1)" tangentialPressure ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"tangentialPressure\"]; })" tangentialPressure ::
#endif
        PointerEventObject ev -> Float
#endif

#if __GHCJS__
#elif (defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH))
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"tiltX\"]; })($1)" tiltX ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"tiltX\"]; })" tiltX ::
#endif
        PointerEventObject ev -> Int
#endif

#if __GHCJS__
#elif (defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH))
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"tiltY\"]; })($1)" tiltY ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"tiltY\"]; })" tiltY ::
#endif
        PointerEventObject ev -> Int
#endif

#if __GHCJS__
#elif (defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH))
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"twist\"]; })($1)" twist ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"twist\"]; })" twist ::
#endif
        PointerEventObject ev -> Int
#endif

#if __GHCJS__
#elif (defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH))
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"altitudeAngle\"]; })($1)" altitudeAngle ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"altitudeAngle\"]; })" altitudeAngle ::
#endif
        PointerEventObject ev -> Double
#endif

#if __GHCJS__
#elif (defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH))
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"azimuthAngle\"]; })($1)" azimuthAngle ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"azimuthAngle\"]; })" azimuthAngle ::
#endif
        PointerEventObject ev -> Double
#endif

#if __GHCJS__
#elif (defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH))
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"pointerType\"]; })($1)" js_pointerType ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"pointerType\"]; })" js_pointerType ::
#endif
        PointerEventObject ev -> JSString
#endif

data PointerType
  = Mouse
  | Pen
  | Touch
  | PointerOther JSString
  deriving (Eq, Ord, Read, Show)

pointerType :: PointerEventObject ev -> PointerType
pointerType peo =
  case js_pointerType peo of
    "mouse" -> Mouse
    "pen"   -> Pen
    "touch" -> Touch
    o       -> PointerOther o

#if __GHCJS__
#elif (defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH))
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"isPrimary\"]; })($1)" isPrimary ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"isPrimary\"]; })" isPrimary ::
#endif
        PointerEventObject ev -> Bool
#endif

-- * extensions to the Element interface

#if __GHCJS__
#elif (defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH))
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => a1[\"setPointerCapture\"](a2))($1,$2)" js_setPointerCapture ::
#else
foreign import javascript unsafe "((a1,a2) => a1[\"setPointerCapture\"](a2))" js_setPointerCapture ::
#endif
        JSElement -> PointerId -> IO ()
#endif

setPointerCapture :: (MonadIO m) => JSElement -> PointerId -> m ()
setPointerCapture e pid = liftIO $ js_setPointerCapture e pid

#if __GHCJS__
#elif (defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH))
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => a1[\"releasePointerCapture\"](a2))($1,$2)" js_releasePointerCapture ::
#else
foreign import javascript unsafe "((a1,a2) => a1[\"releasePointerCapture\"](a2))" js_releasePointerCapture ::
#endif
        JSElement -> PointerId -> IO ()
#endif

releasePointerCapture :: (MonadIO m) => JSElement -> PointerId -> m ()
releasePointerCapture e pid = liftIO $ js_releasePointerCapture e pid

#if __GHCJS__
#elif (defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH))
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => { return a1[\"hasPointerCapture\"](a2); })($1,$2)" js_hasPointerCapture ::
#else
foreign import javascript unsafe "((a1,a2) => { return a1[\"hasPointerCapture\"](a2); })" js_hasPointerCapture ::
#endif
        JSElement -> PointerId -> IO Bool
#endif

hasPointerCapture :: (MonadIO m) => JSElement -> PointerId -> m Bool
hasPointerCapture e pid = liftIO $ js_hasPointerCapture e pid
