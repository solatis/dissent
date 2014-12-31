module Dissent.Protocol.Shuffle.Leader where

import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource

import qualified Dissent.Network.Quorum       as NQ
import qualified Dissent.Types                as T
import qualified Network.Socket               as NS

run :: T.Quorum -> IO ()
run quorum = runResourceT $ do
  sockets <- phase1 quorum

  return ()

-- | In the first phase, our leader will open a socket that
--   slaves can connect to, and will wait for all slaves to
--   connect to the quorum.
--
--   This is a blocking operation.
phase1 :: T.Quorum    -- ^ The Quorum we operate on
       -> ResourceT IO [(NS.Socket, NS.SockAddr)] -- ^ The sockets we accepted
phase1 quorum = NQ.accept quorum T.Leader
