-- | Shuffle protocol

module Dissent.Protocol.Shuffle where

import           Control.Concurrent           (newEmptyMVar, putMVar, readMVar)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource

import qualified Dissent.Network.Quorum       as NQ
import qualified Dissent.Quorum               as Q
import qualified Dissent.Types                as T

run :: T.Quorum -> ResourceT IO ()
run quorum = phase1 (Q.selfPeerType quorum) (quorum)

-- | The first phase is that the peers establish connections
--   with each other. First of all, all slaves will establish
--   a connection with the leader. Furthermore, each slave
--   will establish a connection with his 'successor' in the
--   quorum ring.
phase1 :: T.PeerType  -- ^ Are we a leader or a slave ?
       -> T.Quorum    -- ^ The Quorum we operate on
       -> ResourceT IO ()

-- | Specialization for leader, which will, in addition to
--   acting like a regular slave, accept connections from all
--   nodes in the quorum.
phase1 T.Leader quorum =
  let setupNetwork = do
        mutex <- liftIO (newEmptyMVar)
        _ <- resourceForkIO $ do
          sockets <- NQ.accept quorum T.Leader
          liftIO $ putMVar mutex sockets

        return (mutex)

  in do
    -- Asynchronously starts listening for all connections
    slavesMutex <- setupNetwork

    -- Now setup ourselves as a slave too
    phase1 T.Slave quorum

    -- Wait until all slaves have connected
    slaves <- liftIO $ readMVar slavesMutex

    return ()

-- | Specialization for slaves, which will accept a connection
--   from its predecessor, establish a connection with its
--   successor.
phase1 T.Slave  quorum =
  let acceptPredecessor = do
        mutex <- liftIO (newEmptyMVar)
        _ <- resourceForkIO $ do
          sockets <- NQ.accept quorum T.Leader
          liftIO $ putMVar mutex sockets

        return (mutex)

      connectSuccessor = NQ.connect quorum T.Slave NQ.Infinity

  in do
    -- Asynchronously starts listening for connection of predecessor
    predecessorMutex <- acceptPredecessor

    -- Connect to our successor
    successor <- connectSuccessor

    -- Wait until our predecessor has connected
    predecessor <- liftIO $ readMVar predecessorMutex

    return ()
