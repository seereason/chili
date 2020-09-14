{-# LANGUAGE ExtendedDefaultRules, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TypeFamilies, RankNTypes, DataKinds #-}
module Dominator.HSX where

import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack)
import GHCJS.Marshal.Pure (pFromJSVal)
import GHCJS.Types (JSVal(..), JSString(..))
import Dominator.Types (Attr(Attr, EL), Html(Element, CData), flattenCData, descendants)
import qualified Data.JSString as JS
import qualified Data.JSString.Text as JS

default (Text)

{- HSX2HS -}

genElement (d, t) a c =
    let c' = (concat c)
    in Element t Nothing a (flattenCData c')

genEElement (d, t) a = genElement (d, t) a []

fromStringLit = pack

class AsChild c where
    asChild :: c -> [Html]

instance AsChild Text where
    asChild t = [CData t]

instance AsChild String where
    asChild t = [CData (pack t)]

instance AsChild Html where
    asChild t = flattenCData [t]

instance AsChild [Html] where
    asChild t = flattenCData t

data KV k v = k := v
    deriving (Eq, Ord, Read, Show)

class AsAttr a where
    asAttr :: a -> Attr

instance AsAttr (KV Text Text) where
    asAttr (k := v) = Attr k v

instance AsAttr (KV Text JS.JSString) where
    asAttr (k := v) = Attr k (JS.textFromJSString v)

{-
instance AsAttr model (KV Text model) where
    asAttr (type' := model) =
        case type' of
          "onchange" -> Event Change (const $ pure model)
          "onclick"  -> Event Click  (const $ pure model)
          "oninput"  -> Event Input  (const $ pure model)
--          "onblur"   -> Event Blur   (const $ pure model)
          _ -> error $ "unsupported event: " ++ (unpack type')
-}
{-
instance AsAttr model (KV Text (JSVal -> model)) where
    asAttr (type' := model) =
        case type' of
          "onchange" -> Event Change (model . pFromJSVal)
--          "onclick"  -> Event Click  model
--          "oninput"  -> Event Input  model
--          "onblur"   -> Event Blur   model
          _ -> error $ "unsupported event: " ++ (unpack type')
-}
{-
instance AsAttr model (KV Text (IO String, (String -> model), model)) where
    asAttr (type' := model) =
        case type' of
          "onchange" -> Event (Change, model)
          "onclick"  -> Event (Click, model)
          "oninput"  -> Event (Input, model)
          _ -> error $ "unsupported event: " ++ (unpack type')
-}
instance AsAttr Attr where
    asAttr a = a
