-- GHC JS backend compatibility shim for Data.JSString
-- JSString wraps JSVal so it is FFI-compatible (not [Char]).
module Data.JSString
  ( JSString(..)
  , pack
  , unpack
  , textFromJSString
  , textToJSString
  ) where

import qualified Data.Text as Text
import GHC.JS.Prim (JSVal, toJSString, fromJSString)

newtype JSString = JSString { unJSString :: JSVal }

pack :: String -> JSString
pack = JSString . toJSString

unpack :: JSString -> String
unpack = fromJSString . unJSString

textFromJSString :: JSString -> Text.Text
textFromJSString = Text.pack . unpack

textToJSString :: Text.Text -> JSString
textToJSString = pack . Text.unpack
