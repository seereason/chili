{-# language GADTs #-}
{-# language FlexibleContexts #-}
module Dominator where

import Chili.Types (EventObjectOf(..), IsEvent(..), IsJSNode(toJSNode), JSDocument, JSElement, JSTextNode, JSNode, addEventListener, currentDocument, getElementById, removeChildren, setAttribute, setProperty)
import Control.Concurrent.MVar (newEmptyMVar, putMVar)
import Control.Monad.Trans (MonadIO(liftIO))
import Data.JSString (JSString)
import qualified Data.JSString as JS
import Data.JSString.Text (textToJSString, textFromJSString)
import Data.Text (Text)
import Dominator.Types
import Dominator.Patch (renderHtml)
import GHCJS.Marshal (ToJSVal(..), FromJSVal(..))

{-

Dominator is callback driven -- everything happens in a callback.

Dominator does not provide any special mechanism for sharing state
between callbacks -- just use STM or an MVar.

Components are implemented by simply having them emit events like any
other DOM element.

There is a VDOM -- and the DOM is updated explicitly by the
developer. This makes it possible to perform multiple DOM updates in a
single callback. It also makes it trivial to not update the DOM at
all.
-}

attachById :: JSString -> IO (Maybe DHandle)
attachById elemId =
  do (Just d) <- currentDocument
     me <- getElementById d elemId
     case me of
       Nothing  -> pure Nothing
       (Just e) ->
         do mvdom <- newEmptyMVar
            pure $ Just $ DHandle { root = e
                                  , vdom = mvdom
                                  , doc = d
                                  }


initView :: DHandle -> Html -> IO ()
initView (DHandle root vdom doc) html =
  do node <- renderHtml doc html
     removeChildren root
     appendChild root node
     putMVar vdom html
     pure ()
