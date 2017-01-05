module Chili.Loop where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (TMVar, newTMVar, newEmptyTMVar, takeTMVar, putTMVar)
import Chili.Diff
import Chili.Patch
import Chili.Types

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
