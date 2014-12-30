-- | Network utility functions for Dissent
--
--   Since we are using quite a different aproach than regular networking, we
--   provide a few utility functions here. Specifically, we will run a server
--   that accepts only a single connection and then closes. Furthermore, we
--   provide a function that attempts to connect to a remote host, possibly
--   indefinitely, and blocks until a connection has been established.

module Dissent.Network.Socket where

import Control.Exception (IOException)
import Control.Exception.Lifted (try)

import Control.Monad.Trans.Resource
import Control.Monad.IO.Class (liftIO)

import qualified Network.Socket               as NS

import qualified Dissent.Internal.Debug       as D

create :: NS.Family -> ResourceT IO NS.Socket
create family = do
  (_releaseKey, socket) <- allocate
    (NS.socket family NS.Stream NS.defaultProtocol)
    (NS.close)

  return (socket)

-- | Binds a socket for listening onto a certain address
listen :: NS.PortNumber          -- ^ The port we'll be listening at
       -> ResourceT IO NS.Socket -- ^ Our resulting socket
listen port =
  let addr  = NS.SockAddrInet6 port 0 NS.iN6ADDR_ANY 0

  in do
    socket  <- create NS.AF_INET6
    liftIO $ do
      NS.bind   socket addr
      NS.listen socket 5

    D.log ("Now listening on socket: " ++ show socket) (return (socket))

-- | Accepts a single socket. This is a blocking operation.
accept :: NS.Socket                             -- ^ Server socket to accept connections from.
                                                --   This socket must be already bound using listenSocket.
       -> ResourceT IO (NS.Socket, NS.SockAddr) -- ^ Our accepted connection
accept = liftIO . NS.accept

-- | Utility function that allows us to connect to a remote host, and returns an
--   Either if there is a connection failure.
--
--   This is a blocking operation.
connect :: NS.HostName -> NS.PortNumber -> ResourceT IO (Either IOException (NS.Socket, NS.SockAddr))
connect host port =
  let portToService :: NS.PortNumber -> NS.ServiceName
      portToService = show

      addrInfo :: IO NS.AddrInfo
      addrInfo = do
          (addr:_) <- D.log ("Resolving host " ++ show host ++ " and port " ++ show port) (NS.getAddrInfo Nothing (Just host) (Just (portToService port)))
          D.log ("Resolved address: " ++ show addr) (return addr)

      sockAddr :: NS.AddrInfo -> NS.SockAddr
      sockAddr addr = NS.addrAddress addr

      connect' :: ResourceT IO (NS.Socket, NS.SockAddr)
      connect' = do
        info   <- liftIO $ addrInfo
        socket <- create (NS.addrFamily info)

        let addr = sockAddr info

        D.log ("Now connecting to socket at address: " ++ show addr) (liftIO $ NS.connect socket addr)

        return (socket, addr)

  in try connect'
