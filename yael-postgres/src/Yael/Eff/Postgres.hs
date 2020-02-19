module Yael.Eff.Postgres where

import Yael.Eff
import Database.PostgreSQL.Simple
import Data.Pool
import Control.Monad.Trans.Control

newtype Sql (m :: * -> *) = Sql
  { _withSqlConn :: forall a . (Connection -> m a) -> m a
  }

withSqlConn :: (HasEff Sql f) => (Connection -> EffT f m a) -> EffT f m a
withSqlConn f = withEffT' $ \lower Sql{_withSqlConn} -> _withSqlConn $ lower . f

connSql :: Connection -> Sql m
connSql conn = Sql
  { _withSqlConn = \f -> f conn
  }

poolSql :: (MonadBaseControl IO m) => Pool Connection -> Sql m
poolSql pool = Sql
  { _withSqlConn = withResource pool
  }
