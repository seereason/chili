{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language ExistentialQuantification #-}
{-# language TypeFamilies #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

{-

run:

 ghcjs -i.. DominatorMain.hs

And then open DominatorMain.html in your broswer.

-}
module Main where

import Chili.Types (MouseEvent(Click))
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar, modifyTVar')

import Data.Maybe (isNothing)
import Dominator
import Dominator.Types (Attr(..), Html(..), DHandle)
import Dominator.HSX
import Dominator.Patch
import Dominator.Diff
import Language.Haskell.HSX.QQ (hsx)

data Counter = Counter Int

main :: IO ()
main =
    do -- create a standard TVar to share data between callbacks
       mCounter <- atomically $ newTVar (Counter 0)
       -- attach dominator to the node with id="root"
       mdomH <- attachById "root"
       case mdomH of
         Nothing -> error "could not attach"
         -- intialize the view
         (Just domH) -> initView domH =<< view domH mCounter
       pure ()


-- a function that creates Html from the current Counter state
view :: DHandle -> TVar Counter -> IO Html
view domH mCounter = do
  (Counter c) <- atomically $ readTVar mCounter
  pure $ [hsx| <div>
                <p>Counter = <% show c %></p>
                <button [EL Click (const $ dec domH mCounter)]>-</button>
                <button [EL Click (const $ inc domH mCounter)]>+</button>
               </div>
             |]

-- a handler that decrements the counter
dec :: DHandle -> TVar Counter -> IO ()
dec domH mCounter =
  do atomically $ modifyTVar' mCounter (\(Counter i) -> Counter (pred i))
     -- note that we must explicity redraw the view
     updateView domH =<< view domH mCounter

-- a handle that increments the counter
inc :: DHandle -> TVar Counter -> IO ()
inc domH mCounter =
  do atomically $ modifyTVar' mCounter (\(Counter i) -> Counter (succ i))
     -- note that we must explicity redraw the view
     updateView domH =<< view domH mCounter
     
