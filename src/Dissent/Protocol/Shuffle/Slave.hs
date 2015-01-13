{-# LANGUAGE FlexibleContexts #-}

module Dissent.Protocol.Shuffle.Slave where

import qualified Data.Binary                  as B
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as BSL

import           Control.Concurrent
import           Control.Monad.Error
import           Control.Monad.Trans.Resource

import qualified Network.Socket               as NS

import qualified Dissent.Crypto.Rsa           as R
import qualified Dissent.Crypto.Random        as R
import qualified Dissent.Internal.Debug       as D
import qualified Dissent.Internal.Util        as U
import qualified Dissent.Network.Quorum       as NQ
import qualified Dissent.Network.Socket       as NS
import qualified Dissent.Quorum               as Q

import qualified Dissent.Types.Quorum         as TQ
import qualified Dissent.Types.Remote         as TR
import qualified Dissent.Types.Peer           as TP
import qualified Dissent.Types.Connection     as TC
import qualified Dissent.Types.Self           as TS

run :: TS.Self -> TQ.Quorum -> BS.ByteString -> IO ()
run self quorum message = runResourceT $ do
  connections <- phase1 quorum
  _ <- phase2 quorum connections message

  _ <- runErrorT $ do
    _ <- phase3 (TS.secret self) quorum connections
    return ()

  return ()

-- | Phase 1 for slaves, which does three things:
--   - accept a connection from its predecessor;
--   - connects to its successor;
--   - connects to the leader and performs handshake.
phase1 :: ( MonadIO m
          , MonadResource m)
       => TQ.Quorum              -- ^ The Quorum we operate on
       -> m TC.Connections -- ^ The Sockets we accepted
phase1 quorum =
  let acceptPredecessor = U.forkResource (NQ.accept quorum TP.Slave)
      connectSuccessor  = NQ.connect quorum TP.Slave  NQ.Infinity
      connectLeader     = NQ.connect quorum TP.Leader NQ.Infinity

      -- | After a connection with a leader has been established, we need to
      --   perform a small handshake. At the moment, this only means we have
      --   to announce our PeerId to the leader.
      handShake :: (Either String (NS.Socket, NS.SockAddr)) -> IO ()
      handShake (Right (leaderSock, _)) = NS.encodeAndSend leaderSock (TQ.selfId quorum)
      handShake _ = error ("Unable to connect to leader")

      -- | Constructs a RemoteConnections object out of the objects we have got
      --   after establishing connections with our remotes.
      remoteConnections :: Either String (NS.Socket, NS.SockAddr)
                        -> (NS.Socket, NS.SockAddr)
                        -> Either String (NS.Socket, NS.SockAddr)
                        -> TC.Connections

      -- Specialization for when all connections could be established.
      remoteConnections (Right (leaderSock, _)) (predecessorSock, _) (Right (successorSock, _)) =
        TC.Connections
          (TC.Connection (Q.lookupLeaderPeer      quorum) leaderSock)
          (TC.Connection (Q.lookupPredecessorPeer quorum) predecessorSock)
          (TC.Connection (Q.lookupSuccessorPeer   quorum) successorSock)

      -- If this specialization is reached, it means that one of the connections
      -- could not be established. Since, at the moment, we block infinitely
      -- until all connections *have* been established, this should never be
      -- and an assertion would be appropriate.
      remoteConnections _ _ _ = error ("Unable to connect to all remote connections")

  in do
    -- Asynchronously starts listening for connection of predecessor
    predecessorMutex <- liftResourceT $ acceptPredecessor

    leaderSock       <- liftResourceT $ connectLeader
    liftIO $ handShake (leaderSock)

    successorSock    <- liftResourceT $ connectSuccessor

    -- Wait until our predecessor has connected
    [predecessorSock] <- liftIO $ readMVar predecessorMutex

    return (remoteConnections leaderSock predecessorSock successorSock)

-- | Phase 2 implements the data submission phase as described in the paper.
phase2 :: (MonadIO m)
       => TQ.Quorum       -- ^ The Quorum we operate on
       -> TC.Connections  -- ^ The sockets we accepted
       -> BS.ByteString   -- ^ The message we want to send
       -> m ()
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

      runEncrypt resolver = encrypt (map resolver (TQ.peers quorum))

  in do
    -- First calculate the prime, which is based on the signing key
    c' <- liftIO $ runEncrypt (TR.publicSigningKey . TP.remote) datum

    -- Now calculate the cipher, which is based on the encryption key, and
    -- uses the final prime as input.
    c  <- liftIO $ runEncrypt (TR.publicEncryptionKey . TP.remote) (R.output (snd (last c')))

    -- Send our encrypted data to the leader.
    liftIO $ NS.encodeAndSend (TC.socket (TC.leader connections)) (snd (last c))

-- | In phase 3 all slaves accept data from its predecessor, decrypt it, and send
--   the decrypted data to its sucessor.
phase3 :: ( MonadIO m
          , MonadError String m)
       => TS.Secret       -- Our secret keys
       -> TQ.Quorum       -- The quorum we operate on
       -> TC.Connections  -- The sockets we accepted
       -> m ()
phase3 secret quorum connections =
      -- The first slave of the quorum expects the message from the leader
  let predecessorSocket :: NS.Socket
      predecessorSocket =
        case (Q.selfIsFirst quorum) of
         True  -> TC.socket (TC.leader connections)
         False -> TC.socket (TC.successor connections)

      -- The last slave in the quorum sends the decrypted ciphers to the leader
      successorSocket :: NS.Socket
      successorSocket =
        case (Q.selfIsLast quorum) of
         True  -> TC.socket (TC.leader connections)
         False -> TC.socket (TC.successor connections)

      receiveCiphers socket = do
        ciphers <- liftIO $ NS.receiveAndDecode socket
        either throwError return ciphers

      -- | Decrypts with signing key
      decrypt :: ( MonadIO m
                 , MonadError String m)
              => R.Encrypted
              -> m R.Encrypted
      decrypt encrypted = do
        bytestring <- liftIO $ R.decrypt (R.private (TS.signingKey secret)) encrypted

        case (B.decodeOrFail (BSL.fromStrict bytestring)) of
         Left  (_, _, msg) -> throwError msg
         Right (_, _, obj) -> return obj

  in do
    ciphers   <- receiveCiphers predecessorSocket
    shuffled  <- liftIO $ R.shuffle ciphers
    decrypted <- mapM decrypt shuffled

    liftIO $ NS.encodeAndSend successorSocket decrypted
