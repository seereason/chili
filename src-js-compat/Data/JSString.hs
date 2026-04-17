-- GHC JS backend compatibility shim for Data.JSString
-- JSString wraps JSVal so it is FFI-compatible (not [Char]).
module Data.JSString
  ( JSString(..)
  , pack
  , unpack
  , textFromJSString
  , textToJSString
  ) where

import Data.String (IsString(..))
import qualified Data.Text as Text
import GHC.JS.Prim (JSVal, toJSString, fromJSString)

newtype JSString = JSString { unJSString :: JSVal }

instance IsString JSString where
  fromString = pack

instance Eq JSString where
  a == b = unpack a == unpack b

instance Ord JSString where
  compare a b = compare (unpack a) (unpack b)

instance Show JSString where
  show = unpack

instance Read JSString where
  readsPrec p s = [(pack x, r) | (x, r) <- readsPrec p s]

pack :: String -> JSString
pack = JSString . toJSString

unpack :: JSString -> String
unpack = fromJSString . unJSString

textFromJSString :: JSString -> Text.Text
textFromJSString = Text.pack . unpack

textToJSString :: Text.Text -> JSString
textToJSString = pack . Text.unpack
