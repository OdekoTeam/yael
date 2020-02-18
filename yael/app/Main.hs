{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens ((^.), makeLenses)
import Yael.Eff
import Yael.Eff.Log
import Yael.Eff.Async
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad
import Control.Monad.Trans.Control
import Control.Concurrent.Lifted

data T m = T
  { _op :: m Int
  , _hop :: m Int -> m [Int]
  , _mop :: m (Maybe Int)
  }

dummyT :: Monad m => T m
dummyT = T
  { _op = return 3
  , _hop = replicateM 10
  , _mop = return Nothing
  }

op :: Int :+ '[T]
op = asksEff >>= _op

hop :: HasEffs '[T] m => m Int -> m [Int]
hop mx = do
  t <- asksEff
  _hop t mx

mop :: Maybe Int :+ '[T]
mop = asksEff >>= _mop


newtype Q m = Q
  { _qop :: m Bool
  }

qop :: Bool :+ '[Q]
qop = asksEff >>= _qop

someFunc
  :: (HasEffs '[T, Q, Log, Async] m
     ,Has Bool m
     )
  => m (Maybe (Int, [Int], [Int], Bool, Bool))
someFunc = runMaybeT $ do
  x <- lift op
  y <- lift $ hop op
  z <- MaybeT $ do
    xs <- hop op
    return $ case even $ sum xs of
      True -> Just xs
      _ -> Nothing
  w <- lift qop
  when w . void . lift . async $ logg "I'm here!"
  d <- lift asksEff
  g <- lift . locallyEff not $ asksEff
  return (x, y, z, d, g)

someFunc' :: (HasEffs '[T] m, MonadPlus m) => m (Int, [Int], [Int])
someFunc' = do
  x <- op
  _ <- mzero
  return (x, [x], [x])

data NoOp (m :: * -> *) = NoOp
 deriving Show

main :: IO ()
main = do
  v <- someFunc
    & runEffT
    $ Q{ _qop = return True}
    :<> dummyT
    :<> Const False
    :<> stdoutLog
    :<> concurrentAsync
  print v
