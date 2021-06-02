{-# language ConstraintKinds #-}
-- | Master debugging flag and messaging functions.
module Chili.Debug (Debug) where

import GHC.Stack (HasCallStack)

-- | This is controlled by a cabal flag and should never be true for a production run.
--  Add  'package alderon2\n    flags: +Debug' to your project.local file.
  
type Debug = HasCallStack

