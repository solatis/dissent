-- | Network utility functions for Dissent
--
--   Since we are using quite a different aproach than regular networking, we
--   provide a few utility functions here. Specifically, we will run a server
--   that accepts only a single connection and then closes. Furthermore, we
--   provide a function that attempts to connect to a remote host, possibly
--   indefinitely, and blocks until a connection has been established.
--
--   The default networking functions all use callbacks, which I personally
--   hate, so I wrote the wrappers in such a way that they do *not* use
--   callbacks.
module Dissent.Network where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Network.Socket hiding (connect, listen, accept)
import Network.Simple.TCP
import Control.Exception

import qualified Dissent.Internal.Debug as D
import qualified Dissent.Types          as T
import qualified Dissent.Quorum         as Q

-- | Converts a port to a ServiceName (required by Network.Simple)
portToServiceName :: Int -> ServiceName
portToServiceName port = show port

-- | Determines what host/port to start accepting connections on, accepts a
--   single connection and then closes the listening port.
--
--   This is a blocking operation.
quorumAccept :: T.Quorum              -- ^ The Quorum we need to start accepting connections from
             -> IO (Socket, SockAddr) -- ^ The socket we accepted
quorumAccept quorum =

      -- This function helps us determine the socket address other nodes knows us by
  let localAddr :: T.Address
      localAddr = T.addr (Q.lookupSelfPeer quorum)

  in do
    acceptedSocket <- newEmptyMVar

    listen
      HostAny
      (portToServiceName (T.port localAddr))
      (\(listenSock, _) -> do
          accept
            listenSock
            (\pair -> do
                -- Since we only expect a single connection attempt, it is safe
                -- to close our listening socket at this point.
                close listenSock

                -- And put our accepted socket in our MVar which synchronizes our
                -- blocking operation.
                D.log ("Accepted connection from upstream node: " ++ show (snd pair)) (putMVar acceptedSocket pair)))

    return =<< readMVar acceptedSocket

-- | Connects to our downstream node. If the server is not available, keeps retrying
quorumConnect :: T.Quorum -- ^ The Quorum we need to lookup our downstream peer from
              -> IO (Either String (Socket, SockAddr))
quorumConnect quorum = quorumConnect' quorum (Infinity) 100000

data ConnectAttempts
  = Infinity
  | Attempts Int
  deriving (Eq, Show)

-- | Implementation of quorumConnect function, with extensive parameters
quorumConnect' :: T.Quorum        -- ^ The Quorum we need to lookup our downstream peer from
               -> ConnectAttempts -- ^ The amount of times we will try to connect
               -> Int             -- ^ The delay (in microseconds) between retries
               -> IO (Either String (Socket, SockAddr))
quorumConnect' quorum retries delay =
  let lookupNextPeer :: T.Address
      lookupNextPeer = T.addr (Q.lookupPeer quorum (Q.successorId quorum))

      connectNext :: IO (Socket, SockAddr)
      connectNext = do
        sock <- newEmptyMVar
        connect
          (T.hostName lookupNextPeer)
          (portToServiceName (T.port lookupNextPeer))
          (\s -> D.log
                   ("Established connection with downstream node: " ++ show (snd s))
                   (putMVar sock s))

        readMVar sock

      connectLoop :: ConnectAttempts -> IO (Either String (Socket, SockAddr))
      connectLoop (Attempts 0) = return (Left ("Unable to connect to remote"))
      connectLoop attemptsLeft =

        let handler :: Either IOException (Socket, SockAddr) -> IO (Either String (Socket, SockAddr))
            handler (Right result) = return (Right result)
            handler (Left _) = do
              -- This means the remote host was not (yet) available, and we should retry
              D.log
                ("Unable to connect to " ++ show lookupNextPeer ++ ", sleeping for " ++ show delay ++ " microseconds, attempt = " ++ show attemptsLeft)
                (threadDelay delay)

              case attemptsLeft of
               Attempts i -> connectLoop (Attempts (i - 1))
               Infinity   -> connectLoop Infinity


        in handler =<< (try (connectNext) :: IO (Either IOException (Socket, SockAddr)))

  in connectLoop retries
