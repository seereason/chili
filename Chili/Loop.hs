{-# language ScopedTypeVariables #-}
{-# language RecursiveDo #-}

module Chili.Loop where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (TMVar, newTMVar, newEmptyTMVar, takeTMVar, putTMVar)
import Chili.Diff
import Chili.Patch
import Chili.Types
import Data.Aeson (FromJSON, ToJSON, decodeStrict, encode)
import Data.JSString.Text (textToJSString)
import qualified Data.JSString as JS
import Data.Text (Text)
import JavaScript.Web.MessageEvent (MessageEvent(..))

{-
-- this solution does not work because the event handler code is run
*after* the event handler has returned, so things like preventDefault
do not work.

loop :: JSDocument
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
--              print (model, oldModel)
              let newHtml = view model
                  patches = diff oldHtml (Just newHtml)
              print patches
              apply loop (updateModel modelV) doc body oldHtml patches
              loop' modelV model newHtml
--         updateModel :: TMVar model -> (model -> IO model) -> IO ()
         updateModel modelV f = atomically $ putTMVar modelV f
-}

loop :: (ToJSON remote) =>
        JSDocument
     -> JSNode
     -> model
     -> ((remote -> IO ()) -> model -> IO model)
     -> JS.JSString
     -> (MessageEvent -> model -> IO model)
     -> ((remote -> IO ()) -> model -> Html model)
     -> IO ()
loop doc body initModel initAction url handleWS view =
  do putStrLn "loop"
     modelV <- atomically $ newEmptyTMVar -- (initModel, html)
     rec sendWS <- initRemoteWS url (\me -> updateModel modelV sendWS (handleWS me))
     let html = view sendWS initModel
     atomically $ putTMVar modelV (initModel, html)
     (Just node) <- renderHtml loop (updateModel modelV sendWS) doc html
     removeChildren body
     appendChild body (Just node)
     updateModel modelV sendWS (initAction sendWS)
     pure ()
       where
         updateModel modelV sendWS f =
           do (oldModel, oldHtml) <- atomically $ takeTMVar modelV
              model <- f oldModel
              let newHtml = view sendWS model
                  patches = diff oldHtml (Just newHtml)
              atomically $ putTMVar modelV (model, newHtml)
              print patches
              apply loop (updateModel modelV sendWS) doc body oldHtml patches
              pure ()
{-
         loop' doc body initModel initAction view =
           do loop doc body initModel initAction view
              pure ()
-}



--     -> (model -> Html model)
--     updateModel modelV initAction

{-
     loop' modelV html
       where
         loop' modelV oldHtml =
           do putStrLn "loop'"

              oldModel <- atomically $ takeTMVar modelV
              model <- f oldModel
--              print (model, oldModel)
              let newHtml = view model
                  patches = diff oldHtml (Just newHtml)
              print patches
              apply loop (updateModel modelV) doc body oldHtml patches
              loop' modelV model newHtml
--         updateModel :: TMVar model -> (model -> IO model) -> IO ()
         updateModel modelV f = atomically $ putTMVar modelV f

-}
