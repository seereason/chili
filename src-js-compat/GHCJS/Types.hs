{-# LANGUAGE DefaultSignatures, FlexibleInstances, TypeSynonymInstances #-}
-- GHC JS backend compatibility shim for GHCJS.Types
module GHCJS.Types
  ( IsJSVal(..)
  , JSVal
  , JSString
  , nullRef
  , isNull
  , isUndefined
  ) where

import Data.Coerce (coerce, Coercible)
import GHC.JS.Prim (JSVal, isNull, isUndefined, jsNull)
import Data.JSString (JSString(..))

nullRef :: JSVal
nullRef = jsNull

class IsJSVal a where
  jsval :: a -> JSVal
  default jsval :: Coercible a JSVal => a -> JSVal
  jsval = coerce

instance IsJSVal JSVal where
  jsval = id
