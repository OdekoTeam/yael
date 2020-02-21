module Yael.Eff.Async where

import Yael.Eff
import Control.Concurrent.Lifted
import Control.Monad.Trans.Control

newtype Async m = Async
  { _async :: m () -> m ThreadId
  }


concurrentAsync :: (MonadBaseControl IO m) => Async m
concurrentAsync = Async fork

async :: (HasEff Async f m) => EffT f m () -> EffT f m ThreadId
async m = withEffT' $ \lower Async{_async} -> _async (lower m)
