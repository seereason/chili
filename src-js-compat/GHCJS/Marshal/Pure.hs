{-# LANGUAGE DefaultSignatures, FlexibleInstances #-}
-- GHC JS backend compatibility shim for GHCJS.Marshal.Pure
module GHCJS.Marshal.Pure
  ( PToJSVal(..)
  , PFromJSVal(..)
  , pToJSVal
  , pFromJSVal
  ) where

import Data.Coerce (coerce, Coercible)
import GHC.JS.Prim (JSVal)

class PToJSVal a where
  pToJSVal :: a -> JSVal
  default pToJSVal :: Coercible a JSVal => a -> JSVal
  pToJSVal = coerce

class PFromJSVal a where
  pFromJSVal :: JSVal -> a
  default pFromJSVal :: Coercible JSVal a => JSVal -> a
  pFromJSVal = coerce

instance PToJSVal JSVal where
  pToJSVal = id

instance PFromJSVal JSVal where
  pFromJSVal = id
