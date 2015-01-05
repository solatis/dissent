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
import qualified Dissent.Types                as T


-- | A function that resolves a port we communicate at, based on a peer's role.
--
--   A leader role needs to listen to a different port, since a leader acts
--   as both a leader and a slave, and it is useful to be able to distinguish
--   the connections.
port :: T.Remote -> T.PeerType -> NS.PortNumber
port addr T.Leader = (T.port addr) + 1000
port addr T.Slave  = T.port addr

-- | Determines what host/port to start accepting connections on, accepts
--   connections and then closes the listening port. It allows you to specify
--   the amount of connections you wish to accept.
--
--   This is a blocking operation.
accept :: T.Quorum                                -- ^ The Quorum we need to start accepting connections from
       -> T.PeerType                              -- ^ The role we are accepting connections from. Since a leader
                                                  --   has a double role as Leader and Slave, this parameter must
                                                  --   be explicitly provided.
       -> ResourceT IO [(NS.Socket, NS.SockAddr)] -- ^ The sockets we accepted
accept quorum peerType =

      -- This function helps us determine the socket address other nodes knows us by
  let localAddr    :: T.Remote
      localAddr    = T.remote (Q.lookupSelfPeer quorum)

      -- The amount of connections we will accept. The leader will accept connections
      -- from all nodes in the quorum (including itself), while a slave only accepts
      -- a connection from its predecessor.
      num          :: T.PeerType -> Int
      num T.Leader = length (T.peers quorum)
      num T.Slave  = 1

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
connect :: T.Quorum        -- ^ The Quorum we need to lookup our downstream peer from
        -> T.PeerType      -- ^ Role of the peer we are connecting to
        -> ConnectAttempts -- ^ The amount of times we retry to connect
        -> ResourceT IO (Either String (NS.Socket, NS.SockAddr))
connect quorum peerType connectAttempts =
  let delay      = 100000

      lookupPeer :: T.Remote
      lookupPeer =
        case peerType of
         T.Leader -> T.remote (Q.lookupLeaderPeer    quorum)
         T.Slave  -> T.remote (Q.lookupSuccessorPeer quorum)

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

        in handler =<< (N.connect (T.hostName lookupPeer) remotePort)

  in connectLoop connectAttempts
