-- | Helper functions
module Dissent.Internal.Util where

import           Control.Concurrent
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource

forkResource :: ResourceT IO a -> ResourceT IO (MVar a)
forkResource handler = do
  sync <- liftIO $ newEmptyMVar
  _ <- resourceForkIO $ do
    res <- handler
    liftIO $ putMVar sync res

  return (sync)
