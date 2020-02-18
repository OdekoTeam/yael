module Yael.Eff.Async where

import Yael.Eff
import Control.Concurrent.Lifted
import Control.Monad.Trans.Control

data Async m = Async
  { _async :: m () -> m ThreadId
  }


concurrentAsync :: (MonadBaseControl IO m) => Async m
concurrentAsync = Async fork

async :: (HasEff Async m) => m () -> m ThreadId
async m = do
  a <- asksEff
  _async a m
