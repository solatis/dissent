-- | Quorum related functionality for our network
module Dissent.Network.Quorum where

import           Control.Concurrent           (threadDelay)
import           Control.Exception            (IOException)

import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource

import qualified Network.Socket               as NS

import qualified Dissent.Internal.Debug       as D
import qualified Dissent.Network.Socket       as N
import qualified Dissent.Quorum               as Q

import qualified Dissent.Types.Quorum         as TQ
import qualified Dissent.Types.Remote         as TR
import qualified Dissent.Types.Peer           as TP


-- | A function that resolves a port we communicate at, based on a peer's role.
--
--   A leader role needs to listen to a different port, since a leader acts
--   as both a leader and a slave, and it is useful to be able to distinguish
--   the connections.
port :: TR.Remote -> TP.Type -> NS.PortNumber
port addr TP.Leader = (TR.port addr) + 1000
port addr TP.Slave  = TR.port addr

-- | Determines what host/port to start accepting connections on, accepts
--   connections and then closes the listening port. It allows you to specify
--   the amount of connections you wish to accept.
--
--   This is a blocking operation.
accept :: TQ.Quorum                                -- ^ The Quorum we need to start accepting connections from
       -> TP.Type                              -- ^ The role we are accepting connections from. Since a leader
                                                  --   has a double role as Leader and Slave, this parameter must
                                                  --   be explicitly provided.
       -> ResourceT IO [(NS.Socket, NS.SockAddr)] -- ^ The sockets we accepted
accept quorum peerType =

      -- This function helps us determine the socket address other nodes knows us by
  let localAddr    :: TR.Remote
      localAddr    = TP.remote (Q.lookupSelfPeer quorum)

      -- The amount of connections we will accept. The leader will accept connections
      -- from all nodes in the quorum (including itself), while a slave only accepts
      -- a connection from its predecessor.
      num          :: TP.Type -> Int
      num TP.Leader = length (TQ.peers quorum)
      num TP.Slave  = 1

  in do
    serverSocket   <- D.log
                        ("Now listening for connections at port " ++ show (port localAddr peerType))
                        (N.listen (port localAddr peerType))

    connections    <- sequence (replicate (num peerType) (N.accept serverSocket))

    return (connections)

-- | Data structure that either represents an Infinite amount of reconnects, or
--   a finite number.
data ConnectAttempts
  = Infinity
  | Attempts Int
  deriving (Eq, Show)

-- | Connects to another node; based on PeerType provided, resolves the correct node
--   to connect to.
connect :: TQ.Quorum        -- ^ The Quorum we need to lookup our downstream peer from
        -> TP.Type      -- ^ Role of the peer we are connecting to
        -> ConnectAttempts -- ^ The amount of times we retry to connect
        -> ResourceT IO (Either String (NS.Socket, NS.SockAddr))
connect quorum peerType connectAttempts =
  let delay      = 100000

      lookupPeer :: TR.Remote
      lookupPeer =
        case peerType of
         TP.Leader -> TP.remote (Q.lookupLeaderPeer    quorum)
         TP.Slave  -> TP.remote (Q.lookupSuccessorPeer quorum)

      connectLoop :: ConnectAttempts -> ResourceT IO (Either String (NS.Socket, NS.SockAddr))
      connectLoop (Attempts 0) = return (Left ("Unable to connect to remote"))
      connectLoop attemptsLeft =

        let handler :: Either IOException (NS.Socket, NS.SockAddr) -> ResourceT IO (Either String (NS.Socket, NS.SockAddr))
            handler (Right result) = return (Right result)
            handler (Left _) = do
              -- This means the remote host was not (yet) available, and we should retry
              D.log
                ("Unable to connect to " ++ show lookupPeer ++ ", sleeping for " ++ show delay ++ " microseconds, attempt = " ++ show attemptsLeft)
                (liftIO $ threadDelay delay)

              case attemptsLeft of
               Attempts i -> connectLoop (Attempts (i - 1))
               Infinity   -> connectLoop Infinity

            remotePort :: NS.PortNumber
            remotePort = port lookupPeer peerType

        in handler =<< (N.connect (TR.hostName lookupPeer) remotePort)

  in connectLoop connectAttempts
