module Yael.Eff.Persistent where

import Yael.Eff
import Yael.Eff.Postgres
import Control.Monad.Reader (mapReaderT)
import Database.Persist.Sql (SqlPersistT, runSqlPool, runSqlConn, SqlBackend)
import Database.Persist.Postgresql (openSimpleConn)
import Data.Pool
import UnliftIO
import Control.Monad.Logger

newtype Persistent m = Persistent
  { _runPersistent :: forall a . SqlPersistT m a -> m a
  }

runPersistent :: (HasEff Persistent f m) => SqlPersistT (EffT f m) a -> EffT f m a
runPersistent p = withEffT' $ \lower Persistent{_runPersistent} -> _runPersistent $ mapReaderT lower p

poolPersistent :: (MonadUnliftIO m) => Pool SqlBackend -> Persistent m
poolPersistent pool = Persistent
  { _runPersistent = flip runSqlPool pool
  }

connPersistent :: MonadUnliftIO m => SqlBackend -> Persistent m
connPersistent conn = Persistent
  { _runPersistent = flip runSqlConn conn
  }

sqlPersistent :: (MonadUnliftIO m, MonadLoggerIO m) => Sql m -> Persistent m
sqlPersistent Sql{_withSqlConn} = Persistent
  { _runPersistent = \p -> _withSqlConn $ \conn -> do
      logFunc <- askLoggerIO
      pConn <- liftIO $ openSimpleConn logFunc conn
      runSqlConn p pConn
  }
