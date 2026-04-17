-- GHC JS backend compatibility shim for GHCJS.Foreign
module GHCJS.Foreign
  ( jsNull
  ) where

import GHC.JS.Prim (jsNull, JSVal)
