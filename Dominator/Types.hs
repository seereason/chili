{-# language DataKinds, FlexibleContexts, GADTs, ScopedTypeVariables, PolyKinds #-}
module Dominator.Types
       ( Html(..)
       , Attr(..)
       , Command(..)
       , JSDocument(..)
       , JSElement(..)
       , JSNode(..)
       , EventName(..)
       , EventObject(..)
       , MouseEvent(..)
       , MouseEventObject(..)
       , UniqEventName
       , addEventListener
       , currentDocument
       , debugStrLn
       , flattenCData
       , descendants
       , DHandle(..)
       , appendChild
       , createJSTextNode
       , createJSElement
       , execCommand
       , fromEventTarget
       , getAttribute
       , getChecked
       , getFirstChild
       , getElementById
       , getElementsByTagName
       , isEqualNode
       , item
       , nextSibling
       , queryCommandState
       , removeChildren
       , setAttribute
       , setChecked
       , setProperty
       , setNodeValue
       , target
       , toJSNode
       ) where

import Control.Concurrent.MVar (MVar)
import Control.Monad.Trans (MonadIO(liftIO))
import Chili.Types (Command(..), EventObjectOf, IsEvent, IsJSNode(toJSNode), JSDocument(..), JSElement(..), JSTextNode(..), JSNode(..), EventName(..), EventObject(..), MouseEvent(..), MouseEventObject(..), UniqEventName, addEventListener, addEventListenerOpt, currentDocument, eventName, execCommand, fromEventTarget, getAttribute, getChecked, getFirstChild, getElementById, getElementsByTagName, isEqualNode, item, queryCommandState, nextSibling, removeChildren, setAttribute, setChecked, setProperty, setNodeValue, target)
import Data.JSString (JSString)
import qualified Data.JSString as JS
import Data.JSString.Text (textToJSString, textFromJSString)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import GHCJS.Marshal (ToJSVal(..), FromJSVal(..))

debugStrLn :: String -> IO ()
debugStrLn = const $ pure ()
-- debugStrLn = putStrLn

data Attr where
  Attr     :: Text -> Text -> Attr
  Prop     :: Text -> Text -> Attr
  OnCreate :: (JSElement -> IO ()) -> Attr
  EL       :: (KnownSymbol (UniqEventName event), FromJSVal (EventObjectOf event)) => EventName event -> (EventObjectOf event -> IO ()) -> Attr
  ELO      :: (KnownSymbol (UniqEventName event), FromJSVal (EventObjectOf event)) => EventName event -> (Bool, Bool, Bool) -> (EventObjectOf event -> IO ()) -> Attr

instance Show Attr where
  show (Attr a v) = Text.unpack a <> " := " <> Text.unpack v
  show (Prop a v) = "." <> Text.unpack a <> " = " <> Text.unpack v
  show (OnCreate _ ) = "onCreate"
  show (EL e _) = "on" ++ eventName e
  show (ELO e os _) = "on" ++ eventName e ++ show os

data Html where
  Element :: Text -> Maybe Text -> [Attr] -> [Html] -> Html
  CData   :: Text -> Html

instance Show Html where
  show (Element n mKey attrs elems) = "Element " <> Text.unpack n <> " " <> (case mKey of Nothing -> "" ; Just k -> "{ key = " <> Text.unpack k <> " } ") <> show attrs <> " " <> show elems
  show (CData t) = "CData " <> Text.unpack t
--   show (Cntl _ e _) = "Cntl " ++ show e

-- I believe that if we try to `appendChild` several `CData` nodes
-- that the browser will consolidate them into a single node. That
-- throws off our mapping between the VDOM and the DOM. So, we need to
-- do the same
flattenCData :: [Html] -> [Html]
flattenCData (CData a : CData b : rest) = flattenCData (CData (a <> b) : rest)
flattenCData (h : t) = h : flattenCData t
flattenCData [] = []

descendants :: [Html] -> Int
descendants elems = sum [ descendants children | Element _n _key _attrs children <- elems] + (length elems)


data DHandle =
  DHandle { root :: JSElement
          , vdom :: MVar Html
          , doc  :: JSDocument
          }

-- * appendChild

foreign import javascript unsafe "$1[\"appendChild\"]($2)"
        js_appendChild :: JSNode -> JSNode -> IO JSNode

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Node.appendChild Mozilla Node.appendChild documentation>
appendChild :: (MonadIO m, IsJSNode self, IsJSNode newChild) =>
               self
            -> newChild
            -> m JSNode
appendChild self newChild
  = liftIO (js_appendChild (toJSNode self) (toJSNode newChild))

-- * createTextNode

foreign import javascript unsafe "$1[\"createTextNode\"]($2)"
        js_createTextNode :: JSDocument -> JSString -> IO JSTextNode

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Document.createTextNode Mozilla Document.createTextNode documentation>
createJSTextNode :: (MonadIO m) => JSDocument -> Text -> m JSTextNode
createJSTextNode document data'
  = liftIO (js_createTextNode document (textToJSString data'))

-- * createJSElement

foreign import javascript unsafe "$1[\"createElement\"]($2)"
        js_createJSElement ::
        JSDocument -> JSString -> IO JSElement

-- | <https://developer.mozilla.org/en-US/docs/Web/API/JSDocument.createJSElement Mozilla JSDocument.createJSElement documentation>
createJSElement :: (MonadIO m) => JSDocument -> Text -> m JSElement
createJSElement document tagName
  = liftIO (js_createJSElement document (textToJSString tagName))
