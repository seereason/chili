module Chili.Canvas.Types where

import GHCJS.Marshal (ToJSVal(..), FromJSVal(..))
import GHCJS.Types (IsJSVal(..), JSVal, isNull, isUndefined)

newtype JSContext2D = JSContext2D { unJSContext :: JSVal }

instance ToJSVal JSContext2D where
  toJSVal = return . unJSContext
  {-# INLINE toJSVal #-}

instance FromJSVal JSContext2D where
  fromJSVal = return . fmap JSContext2D . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

maybeJSNullOrUndefined :: JSVal -> Maybe JSVal
maybeJSNullOrUndefined r | isNull r || isUndefined r = Nothing
maybeJSNullOrUndefined r = Just r
