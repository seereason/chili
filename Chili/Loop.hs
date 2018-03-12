{-# language ScopedTypeVariables #-}
{-# language RecursiveDo #-}

module Chili.Loop where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (TMVar, newTMVar, newEmptyTMVar, takeTMVar, putTMVar)
import Chili.Diff
import Chili.Internal (debugStrLn)
import Chili.Patch
import Chili.TDVar
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

loop :: forall remote model. (ToJSON remote) =>
        JSDocument
     -> JSNode
     -> model
     -> ((remote -> IO ()) -> TDVar model -> IO ())
     -> Maybe JS.JSString
     -> ((remote -> IO ()) -> MessageEvent -> TDVar model -> IO ())
     -> ((remote -> IO ()) -> model -> Html model)
     -> IO (TDVar model)
loop doc body initModel initAction murl handleWS view =
  do debugStrLn "loop"
     htmlV <- atomically $ newEmptyTMVar -- html -- (initModel, html)
     model <- atomically $ newTDVar initModel
     rec sendWS <- case murl of
                    Nothing -> pure (\_ -> pure ())
                    (Just url) -> initRemoteWS url (\me -> do handleWS sendWS me model
                                                              updateView loop model sendWS htmlV doc body view
                                                   )
     let html = view sendWS initModel
     atomically $ putTMVar htmlV html
     (Just node) <- renderHtml loop model sendWS htmlV doc body html view
     removeChildren body
     appendChild body (Just node)
     initAction sendWS model -- (updateModel modelV sendWS)
     updateView loop model sendWS htmlV doc body view
     pure model
       where
{-
         updateModel :: TMVar (model, Html model) -> (remote -> IO ()) ->  (model -> IO (Maybe model)) -> IO ()
         updateModel modelV sendWS f =
           do old@(oldModel, oldHtml) <- atomically $ takeTMVar modelV
              mmodel <- f oldModel
              case mmodel of
                Nothing -> do -- putStrLn "loop: no change."
                              atomically $ putTMVar modelV old
                              pure ()
                (Just model) ->
                  do -- putStrLn "loop: changed."
                    let newHtml = view sendWS model
                         patches = diff oldHtml (Just newHtml)
                     atomically $ putTMVar modelV (model, newHtml)
                     -- putStrLn $ "patches: " ++ show patches
                     apply loop (updateModel modelV sendWS) doc body oldHtml patches
                     pure ()
-}
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
