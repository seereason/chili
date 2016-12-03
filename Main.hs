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
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.JSString as JS
import Data.JSString.Text (textToJSString, textFromJSString)
import Diff (diff)
import Patch (apply)
import GHCJS.Foreign (jsNull)
import GHCJS.Types (JSVal(..), JSString(..))
import GHCJS.Marshal (ToJSVal(..), FromJSVal(..))
import Types (Attr(..), Control(..), EventTarget(..), EventObject(..), Event(..), IsEventTarget(..), IsEventObject(..), IsEvent(..), EventObjectOf(..), Html(..), JSDocument, JSNode, MouseEvent(..), MouseEventObject(..), js_alert, addEventListener, appendChild, currentDocument, currentTarget, createJSElement, createJSTextNode, dispatchEvent, getElementsByTagName, item, parentNode, removeChildren, setAttribute, toJSNode, maybeJSNullOrUndefined, unJSNode, target, renderHtml, stopPropagation)


-- * Custom button tag which emits `Flicked` instead of `Click`


-- | custom `MyButtonEvent`
data MyButtonEvent = Flicked
 deriving (Eq, Show)

-- | way too much boilerplate
instance IsEvent MyButtonEvent where
  eventToJSString Flicked = "flicked"

instance MkEvent MyButtonEvent where
  mkEvent _ jsval = MyButtonEventObject jsval

newtype MyButtonEventObject = MyButtonEventObject { unMyButtonEventObject :: JSVal }

instance Show MyButtonEventObject where
  show _ = "MyButtonEventObject"

instance ToJSVal MyButtonEventObject where
  toJSVal = return . unMyButtonEventObject
  {-# INLINE toJSVal #-}

instance FromJSVal MyButtonEventObject where
  fromJSVal = return . fmap MyButtonEventObject . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

instance IsEventObject MyButtonEventObject where
  asEventObject (MyButtonEventObject jsval) = EventObject jsval

type instance EventObjectOf MyButtonEvent   = MyButtonEventObject

-- | implementation of myButton
--
-- Note that is uses the model/view pattern
--
-- This button displays how many times it has been clicked
myButton :: Text -> Control MyButtonEvent
myButton msg =
  Control { cmodel = 0
          , cview = \i ->
              Element "button" [EL Click clickHandler]
               [CData (msg <> " " <> T.pack (show i))]
          }
  where
    clickHandler e m = do
      stopPropagation e
      toggleEventObject <- newEvent Flicked True True
      dispatchEvent (target e) toggleEventObject
      pure (succ m)

-- | an app which uses `myButton`
--
-- We have two buttons, which increment the global counter.
-- One regular button and one myButton.
--
-- We do not show the `myButton` until the global counter is >= 2
app :: Int -> Html Int
app model =
  Element "div" []
   ([ Element "p" [] [ CData $ T.pack $ "# clicks: " ++ show model]
   , Element "button" [EL Click (\e m -> pure (succ m))] [CData "click me!"]
   ] ++ (if (model >= 2)
         then [Cntl (myButton "flick me") Flicked (\_ m -> pure $ succ m)]
         else []))

-- | a pretty standard main function
main :: IO ()
main =
  do (Just doc)   <- currentDocument
     (Just nodes) <- getElementsByTagName doc "body"
     (Just body)  <- item nodes 0
     loop doc body 0 app


loop :: (Show model) => JSDocument
     -> JSNode
     -> model
     -> (model -> Html model)
     -> IO ()
loop doc body initModel view =
  do putStrLn "loop"
     modelV <- atomically $ newEmptyTMVar
--     model <- atomically $ readTVar modelV
     let html = view initModel
     (Just node) <- renderHtml loop (updateModel modelV) doc html
     removeChildren body
     appendChild body (Just node)
     loop' modelV initModel html
       where
         loop' modelV oldModel oldHtml =
           do putStrLn "loop'"
              f <- atomically $ takeTMVar modelV
              model <- f oldModel
              print (model, oldModel)
              let newHtml = view model
                  patches = diff oldHtml (Just newHtml)
              print patches
              apply loop (updateModel modelV) doc body oldHtml patches
              loop' modelV model newHtml
         updateModel modelV f = atomically $ putTMVar modelV f


foreign import javascript unsafe "new Event($1, { 'bubbles' : $2, 'cancelable' : $3})"
        js_newEvent :: JSString -> Bool -> Bool -> IO JSVal

class (IsEvent ev) => MkEvent ev where
  mkEvent :: ev -> JSVal -> EventObjectOf ev

newEvent :: forall ev. (IsEvent ev, MkEvent ev) => ev -> Bool -> Bool -> IO (EventObjectOf ev)
newEvent ev bubbles cancelable =
  do let evStr = eventToJSString ev
     jsval <- js_newEvent evStr bubbles cancelable
     pure $ mkEvent ev jsval

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
