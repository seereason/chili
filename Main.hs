{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language ExistentialQuantification #-}
{-# language TypeFamilies #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
module Main where

import Control.Monad.Trans (MonadIO(..))
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (TMVar, newTMVar, newEmptyTMVar, takeTMVar, putTMVar)
import Data.Char (chr)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.JSString as JS
import Data.JSString.Text (textToJSString, textFromJSString)
import Diff (diff)
import Patch (apply)
import GHCJS.Foreign (jsNull)
import GHCJS.Types (JSVal(..), JSString(..))
import GHCJS.Marshal (ToJSVal(..), FromJSVal(..))
import Types (Attr(..), Control(..), EventTarget(..), EventObject(..), Event(..), IsEventTarget(..), IsEventObject(..), IsEvent(..), EventObjectOf(..), Html(..), JSDocument, JSNode, MouseEvent(..), MouseEventObject(..), js_alert, addEventListener, appendChild, currentDocument, currentTarget, createJSElement, createJSTextNode, dispatchEvent, getElementsByTagName, item, parentNode, removeChildren, setAttribute, toJSNode, maybeJSNullOrUndefined, unJSNode, target, renderHtml)

foreign import javascript unsafe "new Event($1)"
        js_newEvent :: JSString -> IO JSVal

class (IsEvent ev) => MkEvent ev where
  mkEvent :: ev -> JSVal -> EventObjectOf ev

newEvent :: forall ev. (IsEvent ev, MkEvent ev) => ev -> IO (EventObjectOf ev)
newEvent ev =
  do let evStr = eventToJSString ev
     jsval <- js_newEvent evStr
     pure $ mkEvent ev jsval

data ToggleEvent = Toggled
 deriving (Eq, Show)

instance IsEvent ToggleEvent where
  eventToJSString Toggled = "flicked"

instance MkEvent ToggleEvent where
  mkEvent _ jsval = ToggleEventObject jsval

newtype ToggleEventObject = ToggleEventObject { unToggleEventObject :: JSVal }

instance Show ToggleEventObject where
  show _ = "ToggleEventObject"

instance ToJSVal ToggleEventObject where
  toJSVal = return . unToggleEventObject
  {-# INLINE toJSVal #-}

instance FromJSVal ToggleEventObject where
  fromJSVal = return . fmap ToggleEventObject . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

instance IsEventObject ToggleEventObject where
  asEventObject (ToggleEventObject jsval) = EventObject jsval

type instance EventObjectOf ToggleEvent   = ToggleEventObject



myButton :: Text -> Control ToggleEvent
myButton msg =
  Control { cmodel = ()
          , cview = \() ->
              Element "button" [EL Click (\e m -> do toggleEventObject <- newEvent Toggled ; putStrLn "dispatching event" ; dispatchEvent (target e) toggleEventObject ; pure ()) ]
               [CData msg]
          }

app :: Int -> Html Int
app model =
  Element "div" []
   ([ Element "p" [] [ CData $ T.pack $ "# clicks: " ++ show model]
   , Element "button" [EL Click (\e m -> pure (succ m))] [CData "click me!"]
--   , Element "button" [EL Click (\e m -> pure (succ m))] [CData "click me!"]
   ] ++ (if (model >= 0) then [Cntl (myButton "flick me") Toggled (\_ m -> pure $ succ m)] else []))

loop :: JSDocument
     -> JSNode
     -> model
     -> (model -> Html model)
     -> IO ()
loop doc body initModel view =
  do modelV <- atomically $ newEmptyTMVar
--     model <- atomically $ readTVar modelV
     let html = view initModel
     (Just node) <- renderHtml loop (updateModel modelV) doc html
     removeChildren body
     appendChild body (Just node)
     loop' modelV initModel html
       where
         loop' modelV oldModel oldHtml =
           do f <- atomically $ takeTMVar modelV
              model <- f oldModel
              let newHtml = view model
                  patches = diff oldHtml (Just newHtml)
              print patches
              apply loop (updateModel modelV) doc body oldHtml patches
              loop' modelV model newHtml
         updateModel modelV f = atomically $ putTMVar modelV f

main :: IO ()
main =
  do (Just doc)   <- currentDocument
     (Just nodes) <- getElementsByTagName doc "body"
     (Just body)  <- item nodes 0
     loop doc body 0 app


{-
data FancyInputEvent
  = FIChanged String
    deriving Show

data FancyInputAction
  = KeyPressed Int
    deriving Show

fancyInput :: Control FancyInputEvent
fancyInput = Control model update view
  where
    model :: String
    model = []

    update :: FancyInputAction -> String -> (String, Maybe FancyInputEvent)
    update fia m =
      case fia of
        KeyPressed i ->
          let str = m ++ [chr i]
          in (str, Just $ FIChanged str)

    view :: String -> Html FancyInputAction
    view m = Element "div" [] []

data Model = Model
  { msg :: String
  }

initialModel = Model { msg = "Initial message" }

data Action = UpdateMessage String

update :: Action -> Model -> Model
update a m =
  case a of
    UpdateMessage str -> m { msg = str }

view :: Model -> Html Action
view m =
  Element "div" []
   [ Element "p" [] [CData (msg m)]
   , Cntl fancyInput (\e -> case e of FIChanged str -> (UpdateMessage str))
   ]

main :: IO ()
main = putStr $ renderHtml (view initialModel)
-}
