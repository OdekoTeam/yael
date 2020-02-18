module Yael.Eff.Log where

import Yael.Eff
import Control.Monad.IO.Class

newtype Log m = Log
  { _log :: String -> m ()
  }

logg :: String -> () :+ '[Log]
logg s = asksEff >>= flip _log s

stdoutLog :: (MonadIO m) => Log m
stdoutLog = Log $ liftIO . putStrLn
