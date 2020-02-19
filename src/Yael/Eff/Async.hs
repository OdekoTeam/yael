module Yael.Eff.Async where

import Yael.Eff
import Control.Concurrent.Lifted
import Control.Monad.Trans.Control

newtype Async m = Async
  { _async :: m () -> m ThreadId
  }


concurrentAsync :: (MonadBaseControl IO m) => Async m
concurrentAsync = Async fork

async :: (HasEff Async m) => m () -> m ThreadId
async m = withEff' $ \lower Async{_async} -> _async (lower m)
