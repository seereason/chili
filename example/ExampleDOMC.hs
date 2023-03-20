{-# language DataKinds #-}
{-# language QuasiQuotes, TemplateHaskell #-}
{-# language OverloadedStrings #-}
{-# language TypeApplications #-}
module Main where

import Control.Monad.Trans (MonadIO)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, writeTVar)
import Control.Concurrent.STM (atomically)
import Chili.Types (unJSNode, getFirstChild, replaceChild, replaceWith, remove, removeChildren, ev)
import Data.Char (toUpper)
import Dominator.Types (JSDocument, JSElement, JSNode, MouseEvent(..), MouseEventObject(..), addEventListener, fromEventTarget, getAttribute, toJSNode, appendChild, currentDocument, removeChildren, target)
import Dominator.DOMC
import Dominator.JSDOM
import Language.Haskell.TH (Name, ExpQ, mkName)
import System.IO (hFlush, stdout, hGetBuffering, hSetBuffering, BufferMode(..))

data Model = Model
 { message    :: String
 , myList     :: [String]
 , clickCount :: Int
 }

initModel :: Model
initModel = Model
 { message    = "hello!"
 , myList     = ["Kittens", "Puppies", "Ducklings"]
 , clickCount = 0
 }

template :: JSDocument -> IO (JSNode, Model -> IO ())
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
           <ol>
             <f-list-item d-map="myList model"></f-list-item>
           </ol>
           <d-if cond="clickCount model > 10">
            <p><span>click count is greater than 10. In fact it is </span><span>{{ show $ clickCount model }}</span></p>
            <p><span>click count is less than or equal to 10. In fact it is only </span><span>{{ show $ clickCount model }}</span></p>
          </d-if>
         </body>
        </html>
      |]
--
{-
template2 :: JSDocument -> JSNode -> IO (JSNode, Model -> IO ())
template2 =
  [domc|<html>
         <head>
          <title>The Greatest Haskell Frontend Library</title>
         </head>
         <body>
           <p>This is the best framework.</p>
           <p>{{ message model }}</p>
           <ol>
             <f-list-item d-map="myList model"></f-list-item>
           </ol>
         </body>
        </html>
      |]
-}

listItem :: JSDocument -> IO (JSNode, String -> IO ())
listItem =
  [domc|<li>{{ model }}</li>|]

main :: IO ()
main =
  do hSetBuffering stdout LineBuffering
     assertDOM -- use jsdom if we are running under node
     (Just d) <- currentDocument
     model <- newTVarIO initModel
     (newNode, update) <- template d -- (toJSNode d)
--     update <- mkUpdate newNode

--     (Just rootNode) <- getFirstChild (toJSNode d)
--     replaceChild (toJSNode d) newNode rootNode
     removeChildren (toJSNode d)
     appendChild (toJSNode d) newNode
--     replaceWith rootNode newNode

     update =<< (atomically $ readTVar model)
     addEventListener d (ev @Click) (clickHandler update model) False
     pure ()

clickHandler :: (Model -> IO ()) -> TVar Model -> MouseEventObject Click -> IO ()
clickHandler update model e =
  do let (Just elem) = fromEventTarget @JSElement (target e)
     mId <- getAttribute elem "id"
     case mId of
       (Just "inc") -> do
         m <- atomically $ readTVar model
         let m' = m { clickCount = succ $ clickCount m
                    , myList = (show (clickCount m)) : (myList m)
                    }
         atomically $ writeTVar model m'
         update m'
       (Just "dec") -> do
         m <- atomically $ readTVar model
         let m' = m { clickCount = pred $ clickCount m
                    , myList = drop 1 (myList m)
                    }
         atomically $ writeTVar model m'
         update m'
       _ -> pure ()
