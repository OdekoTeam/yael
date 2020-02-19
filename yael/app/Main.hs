module Main where

import Yael.Eff
import Yael.Eff.Log
import Yael.Eff.Async
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad

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
op = withEffT _op

hop :: HasEffs '[T] f => EffT f m Int -> EffT f m [Int]
hop mx = withEffT' $ \lower T{_hop} -> _hop (lower mx)

mop :: Maybe Int :+ '[T]
mop = withEffT _mop

newtype Q m = Q
  { _qop :: m Bool
  }

qop :: Bool :+ '[Q]
qop = withEffT _qop

someFunc
  :: (HasEffs '[T, Q, Log, Async] f, Monad m)
  => EffT f m (Maybe (Int, [Int], [Int]))
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
--  d <- lift asksEff
--  g <- lift . locallyEff not $ asksEff
  return (x, y, z)

someFunc' :: (HasEffs '[T] f, MonadPlus m) => EffT f m (Int, [Int], [Int])
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
--    :<> Const False
    :<> stdoutLog
    :<> concurrentAsync
  print v
