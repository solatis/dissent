module Dissent.Protocol.Shuffle.Slave where

import           Control.Concurrent
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource

import qualified Dissent.Network.Quorum       as NQ
import qualified Dissent.Quorum               as Q
import qualified Dissent.Types                as T
import qualified Network.Socket               as NS

run :: T.Quorum -> IO ()
run quorum = runResourceT $ do
  _ <- phase1 quorum
  return ()

-- | Phase 1 for slaves, which does three things:
--   - accept a connection from its predecessor;
--   - connects to its successor;
--   - connects to the leader.
phase1 :: T.Quorum                         -- ^ The Quorum we operate on
       -> ResourceT IO T.RemoteConnections -- ^ The Sockets we accepted
phase1 quorum =
  let acceptPredecessor = do
        mutex <- liftIO (newEmptyMVar)
        _ <- resourceForkIO $ do
          [socket] <- NQ.accept quorum T.Leader
          liftIO $ putMVar mutex socket

        return (mutex)

      connectSuccessor = NQ.connect quorum T.Slave  NQ.Infinity
      connectLeader    = NQ.connect quorum T.Leader NQ.Infinity

      -- | Constructs a RemoteConnections object out of the objects we have got
      --   after establishing connections with our remotes.
      remoteConnections :: Either String (NS.Socket, NS.SockAddr)
                        -> (NS.Socket, NS.SockAddr)
                        -> Either String (NS.Socket, NS.SockAddr)
                        -> T.RemoteConnections

      -- Specialization for when all connections could be established.
      remoteConnections (Right (leaderSock, _)) (predecessorSock, _) (Right (successorSock, _)) =
        T.RemoteConnections
          (T.RemoteConnection (Q.lookupLeaderPeer      quorum) leaderSock)
          (T.RemoteConnection (Q.lookupPredecessorPeer quorum) predecessorSock)
          (T.RemoteConnection (Q.lookupSuccessorPeer   quorum) successorSock)

      -- If this specialization is reached, it means that one of the connections
      -- could not be established. Since, at the moment, we block infinitely
      -- until all connections *have* been established, this should never be
      -- and an assertion would be appropriate.
      remoteConnections _ _ _ = undefined

  in do
    -- Asynchronously starts listening for connection of predecessor
    predecessorMutex <- acceptPredecessor

    leaderSock    <- connectLeader
    successorSock <- connectSuccessor

    -- Wait until our predecessor has connected
    predecessorSock <- liftIO $ readMVar predecessorMutex

    return (remoteConnections leaderSock predecessorSock successorSock)
