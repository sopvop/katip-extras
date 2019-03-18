module Katip.Monadic.Extra
  ( module Katip.Monadic
  , grabKatipEnv
  , runKatipEnv
  ) where

import           Control.Monad.Trans.Reader (runReaderT)

import           Katip
import           Katip.Monadic


grabKatipEnv
  :: KatipContext m
  => m KatipContextTState
grabKatipEnv =
  KatipContextTState
    <$> getLogEnv
    <*> getKatipContext
    <*> getKatipNamespace

runKatipEnv
  :: KatipContextTState
  -> KatipContextT m a
  -> m a
runKatipEnv st act =
  runReaderT (unKatipContextT act) st
