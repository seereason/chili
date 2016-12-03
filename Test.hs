{-# LANGUAGE OverloadedStrings #-}
module Main where

import Diff
import Types (Html(..), Attr(..), Control(..), MouseEvent(Click))

html1a =
  Element "p" [] [CData "foo"]

html1b =
  Element "p" [] [CData "bar"]

html2a = html1a
html2b =
  Element "div" [] [CData "foo"]


html3a =
  Element "p" [] [CData "foo", CData "bar"]
html3b =
  Element "p" [] [CData "foobar"]

myButton :: Control MouseEvent
myButton = Control
  { cmodel = []
  , cview  = Element "button" [] []
  }

html4a =
  Element "p" [] [CData "foo"]
html4b =
  Cntl myButton Click (\e m -> pure m)

main :: IO ()
main =
  do print $ diff html4a (Just html4b)
     pure ()
