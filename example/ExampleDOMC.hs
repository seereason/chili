{-# language QuasiQuotes, TemplateHaskell #-}
{-# language OverloadedStrings #-}
module Main where

import Control.Monad.Trans (MonadIO)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, writeTVar)
import Control.Concurrent.STM (atomically)
import Data.Char (toUpper)
import Dominator.Types (JSDocument, JSElement, JSNode, MouseEvent(..), MouseEventObject(..), addEventListener, getAttribute, toJSNode, appendChild, currentDocument, removeChildren, target)
import Dominator.DOMC
import Dominator.JSDOM
import Language.Haskell.TH (Name, ExpQ, mkName)

data Model = Model
 { message    :: String
 , clickCount :: Int
 }

initModel :: Model
initModel = Model
 { message    = "hello!"
 , clickCount = 0
 }

template :: JSDocument -> IO (Model -> IO ())
template =
  [domc|<html>
         <head>
          <title>The Greatest Haskell Frontend Library</title>
         </head>
         <body>
           <p>This is the best framework.</p>
           <p>{{ message model }}</p>
           <p>{{ map toUpper (message model) }}</p>
           <p><span>The count is currently </span>
              <span>{{ show (clickCount model) }}</span>
           </p>
           <button id="inc">Inc</button>
           <button id="dec">Dec</button>
         </body>
        </html>
      |]

main :: IO ()
main =
  do assertDOM -- use jsdom if we are running under node
     (Just d) <- currentDocument
     model <- newTVarIO initModel
     update <- template d
     update =<< (atomically $ readTVar model)
     addEventListener d Click (clickHandler update model) False
     pure ()

clickHandler :: (Model -> IO ()) -> TVar Model -> MouseEventObject -> IO ()
clickHandler update model e =
  do mId <- getAttribute (target e) "id"
     case mId of
       (Just "inc") -> do
         m <- atomically $ readTVar model
         let m' = m { clickCount = succ $ clickCount m }
         atomically $ writeTVar model m'
         update m'
       (Just "dec") -> do
         m <- atomically $ readTVar model
         let m' = m { clickCount = pred $ clickCount m }
         atomically $ writeTVar model m'
         update m'
       _ -> pure ()
