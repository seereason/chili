{-# language OverloadedStrings #-}
module Dominator.JSDOM where

import Control.Monad.Trans (MonadIO)
import Chili.Types

assertDOM :: (MonadIO m) => m ()
assertDOM =
  do mDoc <- currentDocument
     case mDoc of
         Nothing -> do mJSDOM <- requireJSDOM
                       case mJSDOM of
                         Nothing -> error "unable to require jsdom"
                         (Just jsdom) ->
                           do mWindow <- newJSDOM jsdom "<html><head></head><body></body></html>"
                              case mWindow of
                                Nothing -> do error "unable to require jsdom"
                                (Just w) ->
                                  do setWindow w
         (Just _) -> pure ()
     mWindow <- window
     case mWindow of
         Nothing -> do error "still no window"
         (Just w) -> do md <- document w
                        case md of
                          Nothing -> do error "window does not have a document"
                          (Just d) -> setCurrentDocument d
