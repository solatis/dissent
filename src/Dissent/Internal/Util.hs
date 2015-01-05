-- | Helper functions
module Dissent.Internal.Util where

import           Control.Concurrent
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource

-- | Forks a function that returns a ResourceT, and returns the result
--   in an MVar primitive.
forkResource :: ResourceT IO a -> ResourceT IO (MVar a)
forkResource handler = do
  sync <- liftIO $ newEmptyMVar
  _ <- resourceForkIO $ do
    res <- handler
    liftIO $ putMVar sync res

  return (sync)
