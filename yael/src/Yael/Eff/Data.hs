module Yael.Eff.Data where

import Yael.Eff

newtype Data a m = Data
  { _getData :: a
  }

getData :: (HasEff (Data a) r m) => EffT r m a
getData = withEffT $ \Data{_getData} -> return _getData

localData :: (HasEff (Data a) r m) => (a -> a) -> EffT r m x -> EffT r m x
localData f = localEffT $ Data . f . _getData
