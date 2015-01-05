module Dissent.Protocol.Shuffle.Slave where

import qualified Data.ByteString              as BS
import qualified Data.Vector                  as V

import           Control.Concurrent
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource

import qualified Network.Socket               as NS

import qualified Dissent.Crypto.Rsa           as R
import qualified Dissent.Internal.Debug       as D
import qualified Dissent.Internal.Util        as U
import qualified Dissent.Network.Quorum       as NQ
import qualified Dissent.Network.Socket       as NS
import qualified Dissent.Quorum               as Q
import qualified Dissent.Types                as T

run :: T.Quorum -> BS.ByteString -> IO ()
run quorum message = runResourceT $ do
  connections <- phase1 quorum
  _ <- liftIO $ phase2 quorum connections message

  return ()

-- | Phase 1 for slaves, which does three things:
--   - accept a connection from its predecessor;
--   - connects to its successor;
--   - connects to the leader and performs handshake.
phase1 :: T.Quorum                         -- ^ The Quorum we operate on
       -> ResourceT IO T.RemoteConnections -- ^ The Sockets we accepted
phase1 quorum =
  let acceptPredecessor = U.forkResource (NQ.accept quorum T.Slave)
      connectSuccessor  = NQ.connect quorum T.Slave  NQ.Infinity
      connectLeader     = NQ.connect quorum T.Leader NQ.Infinity

      -- | After a connection with a leader has been established, we need to
      --   perform a small handshake. At the moment, this only means we have
      --   to announce our PeerId to the leader.
      handShake :: (Either String (NS.Socket, NS.SockAddr)) -> IO ()
      handShake (Right (leaderSock, _)) = NS.encodeAndSend leaderSock (T.selfId quorum)
      handShake _ = error ("Unable to connect to leader")

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
      remoteConnections _ _ _ = error ("Unable to connect to all remote connections")

  in do
    -- Asynchronously starts listening for connection of predecessor
    predecessorMutex <- acceptPredecessor

    leaderSock       <- connectLeader
    liftIO $ handShake (leaderSock)

    successorSock    <- connectSuccessor

    -- Wait until our predecessor has connected
    [predecessorSock] <- liftIO $ readMVar predecessorMutex

    return (remoteConnections leaderSock predecessorSock successorSock)

-- | Phase 2 implements the data submission phase as described in the paper.
phase2 :: T.Quorum                         -- ^ The Quorum we operate on
       -> T.RemoteConnections              -- ^ The sockets we accepted
       -> BS.ByteString                    -- ^ The message we want to send
       -> IO ()
phase2 quorum connections datum =

      -- Single encryption pass. Encrypts a datum according to a list of nodes.
      -- Returns an array of tuples with the source datum text and the encrypted
      -- representation.
  let encrypt :: [R.PublicKey] -> BS.ByteString -> IO [(BS.ByteString, R.Encrypted)]
      encrypt []     _     = return ([])
      encrypt (x:xs) msg = do
          encrypted <- D.log
                         ("Now encrypting message of length " ++ show (BS.length msg))
                         (R.encrypt x msg)
          rest      <- encrypt xs (R.output encrypted)

          return ((msg, encrypted) : rest)

      runEncrypt resolver = encrypt (V.toList (V.map resolver (T.peers quorum)))

  in do
    -- First calculate the prime, which is based on the signing key
    c' <- runEncrypt (T.signingKey . T.remote) datum

    -- Now calculate the cipher, which is based on the encryption key, and
    -- uses the final prime as input.
    c  <- runEncrypt (T.signingKey . T.remote) (R.output (snd (last c')))

    -- Send our encrypted data to the leader.
    NS.encodeAndSend (T.socket (T.leader connections)) (snd (last c))

    return ()
