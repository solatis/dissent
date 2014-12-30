-- | Network utility functions for Dissent
--
--   Since we are using quite a different aproach than regular networking, we
--   provide a few utility functions here. Specifically, we will run a server
--   that accepts only a single connection and then closes. Furthermore, we
--   provide a function that attempts to connect to a remote host, possibly
--   indefinitely, and blocks until a connection has been established.

module Dissent.Network where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException)
import Control.Exception.Lifted (try)

import Control.Monad.Trans.Resource
import Control.Monad.Trans.Class (lift)

import qualified Network.Socket               as NS

import qualified Dissent.Internal.Debug       as D
import qualified Dissent.Types                as T
import qualified Dissent.Quorum               as Q

createSocket :: NS.Family -> ResourceT IO NS.Socket
createSocket family = do
  (_releaseKey, socket) <- allocate
    (NS.socket family NS.Stream NS.defaultProtocol)
    (NS.close)

  return (socket)


-- | Binds a socket for listening onto a certain address
listenSocket :: NS.PortNumber          -- ^ The port we'll be listening at
             -> ResourceT IO NS.Socket -- ^ Our resulting socket
listenSocket port =
  let addr  = NS.SockAddrInet6 port 0 NS.iN6ADDR_ANY 0

  in do
    socket  <- createSocket NS.AF_INET6
    lift $ NS.bind   socket addr
    lift $ NS.listen socket 5

    D.log ("Now listening on socket: " ++ show socket) (return (socket))

-- | Accepts a single socket. This is a blocking operation.
acceptConnection :: NS.Socket                             -- ^ Server socket to accept connections from.
                                                          --   This socket must be already bound using listenSocket.
                 -> ResourceT IO (NS.Socket, NS.SockAddr) -- ^ Our accepted connection
acceptConnection = lift . NS.accept

-- | Utility function that allows us to connect to a remote host, and returns an
--   Either if there is a connection failure.
--
--   This is a blocking operation.
connectSocket :: NS.HostName -> NS.PortNumber -> ResourceT IO (Either IOException (NS.Socket, NS.SockAddr))
connectSocket host port =
  let portToService :: NS.PortNumber -> NS.ServiceName
      portToService = show

      addrInfo :: IO NS.AddrInfo
      addrInfo = do
          (addr:_) <- D.log ("Resolving host " ++ show host ++ " and port " ++ show port) (NS.getAddrInfo Nothing (Just host) (Just (portToService port)))
          D.log ("Resolved address: " ++ show addr) (return addr)

      sockAddr :: NS.AddrInfo -> NS.SockAddr
      sockAddr addr = NS.addrAddress addr

      connect :: ResourceT IO (NS.Socket, NS.SockAddr)
      connect = do
        info   <- lift $ addrInfo
        socket <- createSocket (NS.addrFamily info)

        let addr = sockAddr info

        D.log ("Now connecting to socket at address: " ++ show addr) (lift $ NS.connect socket addr)

        return (socket, addr)

  in try connect


-- | Determines what host/port to start accepting connections on, accepts
--   connections and then closes the listening port. It allows you to specify
--   the amount of connections you wish to accept.
--
--   This is a blocking operation.
quorumAccept :: T.Quorum                                -- ^ The Quorum we need to start accepting connections from
             -> Int                                     -- ^ How many connections do we accept ?
             -> ResourceT IO [(NS.Socket, NS.SockAddr)] -- ^ The sockets we accepted
quorumAccept quorum num =

      -- This function helps us determine the socket address other nodes knows us by
  let localAddr :: T.Address
      localAddr = T.addr (Q.lookupSelfPeer quorum)

  in do
    serverSocket  <- listenSocket (T.port localAddr)
    connections   <- sequence (replicate num (acceptConnection serverSocket))

    return (connections)

-- | Similar like quorumAccept, but accepts only a single connection. Useful for
--   non-leaders in the Quorum.
quorumAcceptOne :: T.Quorum                              -- ^ The quorum we accept connections from
                -> ResourceT IO (NS.Socket, NS.SockAddr) -- ^ All the connections we accept
quorumAcceptOne quorum = (return . head) =<< quorumAccept quorum 1

-- | Data structure that either represents an Infinite amount of reconnects, or
--   a finite number.
data ConnectAttempts
  = Infinity
  | Attempts Int
  deriving (Eq, Show)

-- | Connects to our downstream node. If the server is not available, keeps retrying
quorumConnect :: T.Quorum -- ^ The Quorum we need to lookup our downstream peer from
              -> ResourceT IO (Either String (NS.Socket, NS.SockAddr))
quorumConnect quorum = quorumConnect' quorum (Infinity) 100000

-- | Implementation of quorumConnect function, with extensive parameters
quorumConnect' :: T.Quorum        -- ^ The Quorum we need to lookup our downstream peer from
               -> ConnectAttempts -- ^ The amount of times we will try to connect
               -> Int             -- ^ The delay (in microseconds) between retries
               -> ResourceT IO (Either String (NS.Socket, NS.SockAddr))
quorumConnect' quorum retries delay =
  let lookupNextPeer :: T.Address
      lookupNextPeer = T.addr (Q.lookupPeer quorum (Q.successorId quorum))

      connectLoop :: ConnectAttempts -> ResourceT IO (Either String (NS.Socket, NS.SockAddr))
      connectLoop (Attempts 0) = return (Left ("Unable to connect to remote"))
      connectLoop attemptsLeft =

        let handler :: Either IOException (NS.Socket, NS.SockAddr) -> ResourceT IO (Either String (NS.Socket, NS.SockAddr))
            handler (Right result) = return (Right result)
            handler (Left _) = do
              -- This means the remote host was not (yet) available, and we should retry
              D.log
                ("Unable to connect to " ++ show lookupNextPeer ++ ", sleeping for " ++ show delay ++ " microseconds, attempt = " ++ show attemptsLeft)
                (lift $ threadDelay delay)

              case attemptsLeft of
               Attempts i -> connectLoop (Attempts (i - 1))
               Infinity   -> connectLoop Infinity

        in handler =<< (connectSocket (T.hostName lookupNextPeer) (T.port lookupNextPeer))

  in connectLoop retries
