{-# LANGUAGE DefaultSignatures, FlexibleInstances, TypeSynonymInstances #-}
-- GHC JS backend compatibility shim for GHCJS.Marshal
module GHCJS.Marshal
  ( ToJSVal(..)
  , FromJSVal(..)
  , fromJSValUnchecked
  ) where

import Data.JSString (JSString(..))
import GHC.JS.Prim (JSVal, jsNull, fromJSString, toJSString)

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

instance ToJSVal JSString where
  toJSVal (JSString v) = return v

instance FromJSVal JSString where
  fromJSVal = return . Just . JSString

instance ToJSVal String where
  toJSVal = return . toJSString

instance {-# OVERLAPPING #-} FromJSVal String where
  fromJSVal v = return (Just (fromJSString v))

instance FromJSVal Char where
  fromJSVal v = return (case fromJSString v of { (c:_) -> Just c; _ -> Nothing })

foreign import javascript unsafe "(($1) => { return $1; })" js_toDouble   :: Double -> JSVal
foreign import javascript unsafe "(($1) => { return $1; })" js_fromDouble :: JSVal -> Double

instance ToJSVal Double where
  toJSVal = return . js_toDouble

instance FromJSVal Double where
  fromJSVal = return . Just . js_fromDouble

instance FromJSVal a => FromJSVal [a] where
  fromJSVal _ = return Nothing -- stub: JS array iteration not implemented

fromJSValUnchecked :: FromJSVal a => JSVal -> IO a
fromJSValUnchecked v = do
  mv <- fromJSVal v
  case mv of
    Just x  -> return x
    Nothing -> error "fromJSValUnchecked: conversion failed"
