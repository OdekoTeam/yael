module Yael.Eff.Postgres where

import Yael.Eff
import Database.PostgreSQL.Simple
import Data.Pool
import Control.Monad.Trans.Control

newtype Sql (m :: * -> *) = Sql
  { _withSqlConn :: forall a . (Connection -> m a) -> m a
  }

withSqlConn :: (HasEff Sql m) => (Connection -> m a) -> m a
withSqlConn f = do
  Sql{_withSqlConn} <- asksEff
  _withSqlConn f

connSql :: Connection -> Sql m
connSql conn = Sql
  { _withSqlConn = \f -> f conn
  }

poolSql :: (MonadBaseControl IO m) => Pool Connection -> Sql m
poolSql pool = Sql
  { _withSqlConn = withResource pool
  }
