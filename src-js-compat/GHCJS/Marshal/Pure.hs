{-# LANGUAGE DefaultSignatures, FlexibleInstances #-}
-- GHC JS backend compatibility shim for GHCJS.Marshal.Pure
module GHCJS.Marshal.Pure
  ( PToJSVal(..)
  , PFromJSVal(..)
  ) where

import Data.Coerce (coerce, Coercible)
import Data.JSString (JSString(..), textToJSString)
import qualified Data.Text as Text
import GHC.JS.Prim (JSVal, jsNull)

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

-- JSString is a newtype over JSVal
instance PToJSVal JSString where
  pToJSVal (JSString v) = v

instance PToJSVal Text.Text where
  pToJSVal = pToJSVal . textToJSString

instance PFromJSVal JSString where
  pFromJSVal = JSString

-- Maybe JSString
instance PToJSVal a => PToJSVal (Maybe a) where
  pToJSVal Nothing  = jsNull
  pToJSVal (Just a) = pToJSVal a

instance PFromJSVal a => PFromJSVal (Maybe a) where
  pFromJSVal = Just . pFromJSVal
