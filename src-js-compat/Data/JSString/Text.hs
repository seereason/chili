-- GHC JS backend compatibility shim for Data.JSString.Text
module Data.JSString.Text
  ( textToJSString
  , textFromJSString
  ) where

import Data.JSString (JSString, textToJSString, textFromJSString)
