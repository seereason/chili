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

foreign import javascript unsafe "$1[\"pointerId\"]" pointerId ::
        PointerEventObject ev -> PointerId

foreign import javascript unsafe "$1[\"width\"]" width ::
        PointerEventObject ev -> Double

foreign import javascript unsafe "$1[\"height\"]" height ::
        PointerEventObject ev -> Double

foreign import javascript unsafe "$1[\"pressure\"]" pressure ::
        PointerEventObject ev -> Float

foreign import javascript unsafe "$1[\"tangentialPressure\"]" tangentialPressure ::
        PointerEventObject ev -> Float

foreign import javascript unsafe "$1[\"tiltX\"]" tiltX ::
        PointerEventObject ev -> Int

foreign import javascript unsafe "$1[\"tiltY\"]" tiltY ::
        PointerEventObject ev -> Int

foreign import javascript unsafe "$1[\"twist\"]" twist ::
        PointerEventObject ev -> Int

foreign import javascript unsafe "$1[\"altitudeAngle\"]" altitudeAngle ::
        PointerEventObject ev -> Double

foreign import javascript unsafe "$1[\"azimuthAngle\"]" azimuthAngle ::
        PointerEventObject ev -> Double

foreign import javascript unsafe "$1[\"pointerType\"]" js_pointerType ::
        PointerEventObject ev -> JSString

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

foreign import javascript unsafe "$1[\"isPrimary\"]" isPrimary ::
        PointerEventObject ev -> Bool

-- * extensions to the Element interface

foreign import javascript unsafe "$1[\"setPointerCapture\"]($2)" js_setPointerCapture ::
        JSElement -> PointerId -> IO ()

setPointerCapture :: (MonadIO m) => JSElement -> PointerId -> m ()
setPointerCapture e pid = liftIO $ js_setPointerCapture e pid


foreign import javascript unsafe "$1[\"releasePointerCapture\"]($2)" js_releasePointerCapture ::
        JSElement -> PointerId -> IO ()

releasePointerCapture :: (MonadIO m) => JSElement -> PointerId -> m ()
releasePointerCapture e pid = liftIO $ js_releasePointerCapture e pid

foreign import javascript unsafe "$1[\"hasPointerCapture\"]($2)" js_hasPointerCapture ::
        JSElement -> PointerId -> IO Bool

hasPointerCapture :: (MonadIO m) => JSElement -> PointerId -> m Bool
hasPointerCapture e pid = liftIO $ js_hasPointerCapture e pid


