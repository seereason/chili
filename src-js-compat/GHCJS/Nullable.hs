{-# LANGUAGE DefaultSignatures #-}
-- GHC JS backend compatibility shim for GHCJS.Nullable
module GHCJS.Nullable
  ( Nullable(..)
  , nullableToMaybe
  , maybeToNullable
  ) where

import Data.Coerce (coerce, Coercible)
import GHC.JS.Prim (JSVal, jsNull, isNull)
import GHCJS.Marshal.Pure (PFromJSVal(..), PToJSVal(..))

newtype Nullable a = Nullable { unNullable :: JSVal }

nullableToMaybe :: PFromJSVal a => Nullable a -> Maybe a
nullableToMaybe (Nullable v)
  | isNull v  = Nothing
  | otherwise = Just (pFromJSVal v)

maybeToNullable :: PToJSVal a => Maybe a -> Nullable a
maybeToNullable Nothing  = Nullable jsNull
maybeToNullable (Just a) = Nullable (pToJSVal a)
