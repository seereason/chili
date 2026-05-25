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
#elif defined(javascript_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"pointerId\"]; })" pointerId ::
        PointerEventObject ev -> PointerId
#endif

#if __GHCJS__
#elif defined(javascript_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"width\"]; })" width ::
        PointerEventObject ev -> Double
#endif

#if __GHCJS__
#elif defined(javascript_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"height\"]; })" height ::
        PointerEventObject ev -> Double
#endif

#if __GHCJS__
#elif defined(javascript_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"pressure\"]; })" pressure ::
        PointerEventObject ev -> Float
#endif

#if __GHCJS__
#elif defined(javascript_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"tangentialPressure\"]; })" tangentialPressure ::
        PointerEventObject ev -> Float
#endif

#if __GHCJS__
#elif defined(javascript_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"tiltX\"]; })" tiltX ::
        PointerEventObject ev -> Int
#endif

#if __GHCJS__
#elif defined(javascript_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"tiltY\"]; })" tiltY ::
        PointerEventObject ev -> Int
#endif

#if __GHCJS__
#elif defined(javascript_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"twist\"]; })" twist ::
        PointerEventObject ev -> Int
#endif

#if __GHCJS__
#elif defined(javascript_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"altitudeAngle\"]; })" altitudeAngle ::
        PointerEventObject ev -> Double
#endif

#if __GHCJS__
#elif defined(javascript_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"azimuthAngle\"]; })" azimuthAngle ::
        PointerEventObject ev -> Double
#endif

#if __GHCJS__
#elif defined(javascript_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"pointerType\"]; })" js_pointerType ::
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
#elif defined(javascript_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"isPrimary\"]; })" isPrimary ::
        PointerEventObject ev -> Bool
#endif

-- * extensions to the Element interface

#if __GHCJS__
#elif defined(javascript_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => a1[\"setPointerCapture\"](a2))" js_setPointerCapture ::
        JSElement -> PointerId -> IO ()
#endif

setPointerCapture :: (MonadIO m) => JSElement -> PointerId -> m ()
setPointerCapture e pid = liftIO $ js_setPointerCapture e pid

#if __GHCJS__
#elif defined(javascript_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => a1[\"releasePointerCapture\"](a2))" js_releasePointerCapture ::
        JSElement -> PointerId -> IO ()
#endif

releasePointerCapture :: (MonadIO m) => JSElement -> PointerId -> m ()
releasePointerCapture e pid = liftIO $ js_releasePointerCapture e pid

#if __GHCJS__
#elif defined(javascript_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => { return a1[\"hasPointerCapture\"](a2); })" js_hasPointerCapture ::
        JSElement -> PointerId -> IO Bool
#endif

hasPointerCapture :: (MonadIO m) => JSElement -> PointerId -> m Bool
hasPointerCapture e pid = liftIO $ js_hasPointerCapture e pid
