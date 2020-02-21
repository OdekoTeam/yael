{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuantifiedConstraints #-}

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


newtype EffT (f :: (* -> *) -> *) (m :: * -> *) (a :: *) = EffT
  { unEffT :: R.ReaderT (f m) m a
  } deriving newtype ( Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch
                     , MonadMask, MonadPlus, Alternative, MonadBase s, MonadBaseControl s)

runEffT
  :: forall f m a
   . EffT f m a
  -> f m
  -> m a
runEffT (EffT (R.ReaderT r)) y = r y

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

withEffT :: (Monad m, Project f g) => (forall n . Monad n => g n -> n a) -> EffT f m a
withEffT use = withEffT' $ const use

withEffT'
  :: (Project f g, Monad m)
  => (forall n . Monad n => (forall x . EffT f m x -> n x) -> g n -> n a)
  -> EffT f m a
withEffT' use = EffT . R.ReaderT $ \f -> use (\e -> runEffT e f) (f ^. prj)

localEffT :: Project f g => (g m -> g m) -> EffT f m a -> EffT f m a
localEffT modify (EffT (R.ReaderT r)) = EffT . R.ReaderT $ \f -> r $ f & prj %~ modify

mapEffT
  :: Monad m
  => (f m -> g m)
  -> EffT g m a
  -> EffT f m a
mapEffT f (EffT (R.ReaderT r)) = EffT . R.ReaderT $ \g -> r (f g)

class Project (f :: (* -> *) -> *) g where
  prj :: Lens' (f m) (g m)

instance {-# OVERLAPPING #-} Project x x where
  prj = id

infixr :<>

data (a :<> b) (m :: * -> *) = a m  :<> b m
  deriving (Show, Generic)

instance Field1 ((a :<> b) m) ((a' :<> b) m) (a m) (a' m)
instance Field2 ((a :<> b) m) ((a :<> b') m) (b m) (b' m)

{-
instance Project (Const x (b :: (* -> *))) x where
  prj = lens getConst (const Const)
-}

{-
instance {-# OVERLAPPING #-} Project ((Const a :<> b) m) a where
  prj = _1 . prj
-}

instance {-# OVERLAPPING #-} Project ((a :<> b)) a where
  prj = _1

instance {-# OVERLAPPABLE #-} Project b c => Project ((a :<> b)) c where
  prj = _2 . (prj @b @c)


type family MissingError x y where
  MissingError x f =
    'Text "Expected a handler for " ':<>: 'ShowType f ':<>: 'Text " to provided through `runEffT`"
    ':$$: 'Text "The handlers available are: " ':<>: ShowStack (Stacks x)
{-
  MissingError x v =
    'Text "Expected data " ':<>: 'ShowType v ':<>: 'Text " to provided through `runEffT`"
    ':$$: 'Text "The handlers available are: " ':<>: ShowStack (Stacks x)
-}

type family Stacks x where
  Stacks (f :<> g) = f ': (Stacks g)
  Stacks f = '[f]

type family ShowStack (xs :: [k]) where
  ShowStack '[x] = 'ShowType x
  ShowStack (x ': xs) = 'ShowType x ':<>: 'Text ", " ':<>: ShowStack xs

instance
  {-# OVERLAPPABLE #-}
  (TypeError (MissingError x y))  =>
  Project x y where
  prj = error "Missing implementation! This should be a type error"

type HasEff a f m = (Project f a, Monad m)

data AccessType where
  Effect :: f -> AccessType

type family HasOne x f m where
  HasOne ('Effect e) f m = HasEff e f m

type family HasAll xs f m where
  HasAll '[x] f m = HasOne x f m
  HasAll (x ': xs) f m = (HasOne x f m, HasAll xs f m)

type family HasEffs' k (xs :: [k]) f m where
  HasEffs' AccessType xs f m = HasAll xs f m
  HasEffs' ((* -> *) -> *) '[x] f m  = HasOne ('Effect x) f m
  HasEffs' ((* -> *) -> *) (x ': xs) f m = (HasOne ('Effect x) f m, HasEffs' ((* -> *) -> *) xs f m)

type K (x :: [k]) = k

type HasEffs effs f m = HasEffs' (K effs) effs f m

infix 8 :/

type family fs :/ ds where
  '[] :/ '[] = '[]
  (f ': fs) :/ ds = 'Effect f ': (fs :/ ds)


infix 7 :+

type (:+) v effs = forall m f . (HasEffs effs f m) => EffT f m v
