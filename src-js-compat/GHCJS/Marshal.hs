{-# LANGUAGE DefaultSignatures, FlexibleInstances, TypeSynonymInstances #-}
-- GHC JS backend compatibility shim for GHCJS.Marshal
module GHCJS.Marshal
  ( ToJSVal(..)
  , FromJSVal(..)
  ) where

import GHC.JS.Prim (JSVal, jsNull)

class ToJSVal a where
  toJSVal :: a -> IO JSVal
  toJSValListOf :: [a] -> IO JSVal
  toJSValListOf _ = return jsNull -- stub

class FromJSVal a where
  fromJSVal :: JSVal -> IO (Maybe a)
  fromJSValListOf :: JSVal -> IO (Maybe [a])
  fromJSValListOf _ = return Nothing -- stub

instance ToJSVal JSVal where
  toJSVal = return

instance FromJSVal JSVal where
  fromJSVal = return . Just
