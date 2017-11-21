{- similar to a TVar, but with a dirty bit -}
module Chili.TDVar where

import Control.Applicative ((<$>))
import           Control.Concurrent.STM (STM)
import           Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar, modifyTVar)
import qualified Control.Concurrent.STM.TVar as TVar

newtype TDVar a = TDVar (TVar (Bool, a))

newTDVar :: a -> STM (TDVar a)
newTDVar a =
  do v <- newTVar (False, a)
     pure (TDVar v)

readTDVar :: TDVar a -> STM a
readTDVar (TDVar tv) = snd <$> readTVar tv

writeTDVar :: TDVar a -> a -> STM ()
writeTDVar (TDVar tv) a = writeTVar tv (True, a)

modifyTDVar :: TDVar a -> (a -> a) -> STM ()
modifyTDVar (TDVar tv) f = modifyTVar tv (\(_, a) -> (True, f a))

isDirtyTDVar :: TDVar a -> STM Bool
isDirtyTDVar (TDVar tv) = fst <$> readTVar tv

cleanTDVar :: TDVar a -> STM ()
cleanTDVar (TDVar tv) = modifyTVar tv (\(_,a) -> (False, a))

