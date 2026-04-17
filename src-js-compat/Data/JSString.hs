-- GHC JS backend compatibility shim for Data.JSString
-- JSString is String for the GHC JS backend.
module Data.JSString
  ( JSString
  , pack
  , unpack
  , textFromJSString
  , textToJSString
  ) where

import qualified Data.Text as Text

type JSString = String

pack :: String -> JSString
pack = id

unpack :: JSString -> String
unpack = id

textFromJSString :: JSString -> Text.Text
textFromJSString = Text.pack

textToJSString :: Text.Text -> JSString
textToJSString = Text.unpack
