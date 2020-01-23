{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language ExistentialQuantification #-}
{-# language TypeFamilies #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Control.Monad.Trans (MonadIO(..))
import Control.Concurrent.STM (atomically)
import Data.Char (chr)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.JSString as JS
import Data.JSString.Text (textToJSString, textFromJSString)
import Chili.Loop (loop)
import Chili.TDVar (modifyTDVar)
import Chili.Types (Attr(..), Control(..), EventTarget(..), EventObject(..), Event(..), IsEventTarget(..), IsEventObject(..), IsEvent(..), EventObjectOf(..), Html(..), JSDocument, JSNode, MouseEvent(..), MouseEventObject(..), MkEvent(..), js_alert, addEventListener, appendChild, currentDocument, currentTarget, createJSElement, createJSTextNode, dispatchEvent, fromEventTarget, getElementsByTagName, getInnerHTML, item, parentNode, removeChildren, setAttribute, toJSNode, maybeJSNullOrUndefined, newEvent, unJSNode, target, stopPropagation, js_alert, focus)
import Chili.HSX
import GHCJS.Foreign (jsNull)
import GHCJS.Types (JSVal(..), JSString(..))
import GHCJS.Marshal (ToJSVal(..), FromJSVal(..))
import Language.Haskell.HSX.QQ (hsx)
import System.IO (hFlush, stdout)


-- * Custom button tag which emits `Flicked` instead of `Click`

-- | custom `MyButtonEvent`
data MyButtonEvent = Flicked
 deriving (Eq, Show)

-- | way too much boilerplate -- insert template-haskell here?
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

-- end of boilerplate

-- | implementation of myButton
--
-- Note that is uses the model/view pattern
--
-- This button displays how many times it has been clicked
myButton :: Text -> Control MyButtonEvent
myButton msg =
  Control { cmodel = 0
          , cinit = \_ _ -> pure ()
          , cview = \_ i ->
              [hsx|
                 <button [EL Click clickHandler]><% msg <> " " <> T.pack (show i) %></button>
              |]
          }
  where
    clickHandler e m = do
      stopPropagation e
      toggleEventObject <- newEvent Flicked True True
      dispatchEvent (target e) toggleEventObject
      atomically $ modifyTDVar m succ
      pure ()

-- | an app which uses `myButton`
--
-- We have two buttons, which increment the global counter.
-- One regular button and one myButton.
--
-- We do not show the `myButton` until the global counter is >= 2
--
-- Note that the internal state of the myButton counter is not recorded in the global model.
--
-- Note also that we can only attach a single event handler to the
-- embedded control. That is a bug -- there is no fundamental reason
-- for that limitation.
app :: (() -> IO ()) -> Int -> Html Int
app _ model =
  [hsx|
    <div>
      <p><% "# clicks: " ++ show model %></p>
      <button [ EL Click   (\e m -> do let (Just elem) = fromEventTarget (target e)
                                       h <- getInnerHTML elem
                                       print h
                                       hFlush stdout
                                       atomically $ modifyTDVar m succ) ]>click me!</button>
      <% if model >= 2
           then [Cntl (myButton "flick me") Flicked (\_ m -> atomically $ modifyTDVar m succ)]
           else []
       %>
    </div>
   |]
{-
  Element "div" []
   ([ Element "p" [] [ CData $ T.pack $ "# clicks: " ++ show model]
    , Element "button" 
                       ]
      [CData "click me!"]
   ] ++ (if (model >= 2)
         then [Cntl (myButton "flick me") Flicked (\_ m -> atomically $ modifyTDVar m succ)]
         else []) )
-}
-- | a pretty standard main function
main :: IO ()
main =
  do (Just doc)   <- currentDocument
     (Just nodes) <- getElementsByTagName doc "body"
     (Just body)  <- item nodes 0
     loop doc body 0 (\_ _ -> pure ()) Nothing (\_ _ _ -> pure ()) app
     pure ()





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
