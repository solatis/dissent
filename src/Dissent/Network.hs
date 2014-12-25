-- | Main interface to Dissent
module Dissent.Network where

import Data.Maybe ()
import Network hiding (accept)
import Network.Socket
import Network.BSD (getProtocolNumber)
import Control.Concurrent (forkIO)

import qualified Dissent.Internal.Debug as D
import qualified Dissent.Types          as T
import qualified Dissent.Quorum         as Q

-- | Open a socket and start accepting connections, which are processed using a
--   callback function. The callback function is executed in a separate thread,
--   and as such will not interfere with accepting new connections.
--
--   This is a blocking operation.
acceptLoop :: HostName          -- ^ Hostname we will listen at.
           -> PortNumber        -- ^ Port we will listen on.
           -> (Socket -> IO ()) -- ^ Callback function that is called for each incoming connection
           -> IO ()             -- ^ Our output Quorum, with the acceptor bound
acceptLoop host port callback =
  let listenSocket :: IO Socket
      listenSocket = do

        -- Network.Socket appears to be just a thin wrapper over the POSIX C API,
        -- so this code has a very C-like feeling to it, the main thing being that,
        -- instead of returning new 'Socket' objects, the sockets are modified in
        -- place and only return an IO ()

        -- Anyway, first, let's get a SockAddr out of our hostname and port number.
        infos <- getAddrInfo Nothing (Just host) Nothing

        let info     = head infos
            sockAddr = case addrAddress info of
              (SockAddrInet _ addr)          -> SockAddrInet port addr
              (SockAddrInet6 _ _ addr scope) -> SockAddrInet6 port 0 addr scope
              (SockAddrUnix _)               -> error ("Unix Sockets not supported")

        -- We are going to use a TCP connection.
        proto <- getProtocolNumber "tcp"

        -- Create the socket and bind the socket
        sock <- socket (addrFamily info) Stream proto
        bindSocket sock sockAddr
        listen sock maxListenQueue

        -- Yay we're now listening on the socket for incoming connections
        return (sock)

      -- Accepts a new connection, processes in the background, and recursively
      -- accepts next connection.
      acceptNext :: Socket -> IO ()
      acceptNext sock = do
        threadId <- forkIO . callback . fst =<< accept sock

        D.log
          ("Forked new thread with id: " ++ show threadId ++ ", accepting next connection")
          (acceptNext sock)

  in  acceptNext =<< listenSocket

-- | Determines what host/port to start accepting connections on, and runs a never-ending
--   loop accepting said connections.
--
--   This is a blocking operation.
acceptQuorumLoop :: T.Quorum          -- ^ The Quorum we need to start accepting connections from
                 -> (Socket -> IO ()) -- ^ Callback function that is called for each incoming connection
                 -> IO ()             -- ^ Our output Quorum, with the acceptor bound
acceptQuorumLoop quorum callback =

      -- This function helps us determine the socket address other nodes knows us by
  let localAddr :: SockAddr
      localAddr = T.addr (Q.lookupSelfPeer quorum)

      -- This function extracts the host port from the socket address other nodes
      -- use to connect to us, which we can use to determine what to bind to.
      localHostPort :: (HostName, PortNumber)
      localHostPort = case localAddr of
        SockAddrInet  port _     -> ("0.0.0.0", port)
        SockAddrInet6 port _ _ _ -> ("::1",     port)
        SockAddrUnix _           -> error ("Unix Sockets not suported")

  in acceptLoop (fst localHostPort) (snd localHostPort) callback
