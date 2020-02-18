{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}

module Yael.Eff
  ( (&)
  , Const(..)
  , module Yael.Eff
  ) where

import Control.Lens ((^.), Lens', lens, Field1(_1), Field2(_2), (%~))
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import qualified Control.Monad.Reader as R
import Control.Exception.Safe
import UnliftIO (MonadUnliftIO(..), withUnliftIO, UnliftIO(..))
import Data.Function ((&))
import GHC.TypeLits
import GHC.Generics
import Control.Monad.Base
import Control.Monad.Trans.Control

newtype EffT f m a = EffT
  { unEffT :: R.ReaderT (f (EffT f m)) m a
  } deriving newtype ( Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch
                     , MonadMask, MonadPlus, Alternative, MonadBase s, MonadBaseControl s)

runEffT :: EffT f m a -> f (EffT f m) -> m a
runEffT = R.runReaderT . unEffT

instance MonadUnliftIO m => MonadUnliftIO (EffT f m) where
  {-# INLINE askUnliftIO #-}
  askUnliftIO = EffT . R.ReaderT $ \r ->
                withUnliftIO $ \u ->
                return (UnliftIO (unliftIO u . flip runEffT r))
  {-# INLINE withRunInIO #-}
  withRunInIO inner =
    EffT . R.ReaderT $ \r ->
    withRunInIO $ \run ->
    inner (run . flip runEffT r)

instance MonadTrans (EffT f) where
  lift = EffT . lift

askEffT :: Monad m => EffT f m (f (EffT f m))
askEffT = EffT R.ask

localEffT :: (f (EffT f m) -> f (EffT f m)) -> EffT f m a -> EffT f m a
localEffT f m = EffT . R.ReaderT $ runEffT m . f

class Monad m => MonadEff m where
  type F m :: (* -> *) -> *

  askEff :: m (F m m)

  localEff :: (F m m -> F m m) -> m a -> m a

instance (Monad m) => MonadEff (EffT f m) where
  type F (EffT f m) = f

  askEff = askEffT

  localEff = localEffT

class Project f g where
  prj :: Lens' f g

instance {-# OVERLAPPING #-} Project x x where
  prj = id

infixr :<>

data (a :<> b) (m :: * -> *) = a m  :<> b m
  deriving (Show, Generic)

instance Field1 ((a :<> b) m) ((a' :<> b) m) (a m) (a' m)
instance Field2 ((a :<> b) m) ((a :<> b') m) (b m) (b' m)

instance Project (Const x (b :: (* -> *))) x where
  prj = lens getConst (const Const)

instance {-# OVERLAPPING #-} Project ((Const a :<> b) m) a where
  prj = _1 . prj

instance {-# OVERLAPPING #-} Project ((a :<> b) m) (a m) where
  prj = _1

instance {-# OVERLAPPABLE #-} Project (b m) c => Project ((a :<> b) m) c where
  prj = _2 . (prj @(b m) @c)


type family MissingError x y where
  MissingError x (f (m :: * -> *)) =
    'Text "Expected a handler for " ':<>: 'ShowType f ':<>: 'Text " to provided through `runEffT`"
    ':$$: 'Text "The handlers available are: " ':<>: ShowStack (Stacks x)
  MissingError x v =
    'Text "Expected data " ':<>: 'ShowType v ':<>: 'Text " to provided through `runEffT`"
    ':$$: 'Text "The handlers available are: " ':<>: ShowStack (Stacks x)

type family Stacks x where
  Stacks ((f :<> g) m) = f ': (Stacks (g m))
  Stacks (f m) = '[f]

type family ShowStack (xs :: [k]) where
  ShowStack '[x] = 'ShowType x
  ShowStack (x ': xs) = 'ShowType x ':<>: 'Text ", " ':<>: ShowStack xs

instance
  {-# OVERLAPPABLE #-}
  (TypeError (MissingError x y))  =>
  Project x y where
  prj = error "Missing implementation! This should be a type error"

type Has a m = (Project (F m m) a, MonadEff m)

type HasEff f m = Has (f m) m

asksEff :: (Has a m) => m a
asksEff = (^. prj) <$> askEff

locallyEff :: (Has a m) => (a -> a) -> m x -> m x
locallyEff f mx = localEff (prj %~ f)  mx

data AccessType where
  Effect :: f -> AccessType
  Data :: a -> AccessType

type family HasOne x m where
  HasOne ('Effect f) m = HasEff f m
  HasOne ('Data a) m = Has a m

type family HasAll xs m where
  HasAll '[x] m = HasOne x m
  HasAll (x ': xs) m = (HasOne x m, HasAll xs m)

type family HasEffs' k (xs :: [k]) m where
  HasEffs' AccessType xs m = HasAll xs m
  HasEffs' ((* -> *) -> *) '[x] m = HasOne ('Effect x) m
  HasEffs' ((* -> *) -> *) (x ': xs) m = (HasOne ('Effect x) m, HasEffs' ((* -> *) -> *) xs m)

type K (x :: [k]) = k

type HasEffs effs m = HasEffs' (K effs) effs m

infix 8 :/

type family fs :/ ds where
  '[] :/ '[] = '[]
  (f ': fs) :/ ds = 'Effect f ': (fs :/ ds)
  '[] :/ (d ': ds) = 'Data d ': ('[] :/ ds)

infix 7 :+

type (:+) v effs = forall m . (HasEffs effs m) => m v
