-- | Quorum related functionality for our network
module Dissent.Network.Quorum where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException)

import Control.Monad.Trans.Resource
import Control.Monad.IO.Class (liftIO)

import qualified Network.Socket               as NS

import qualified Dissent.Network.Socket       as N
import qualified Dissent.Internal.Debug       as D
import qualified Dissent.Types                as T
import qualified Dissent.Quorum               as Q

-- | Determines what host/port to start accepting connections on, accepts
--   connections and then closes the listening port. It allows you to specify
--   the amount of connections you wish to accept.
--
--   This is a blocking operation.
accept :: T.Quorum                                -- ^ The Quorum we need to start accepting connections from
       -> Int                                     -- ^ How many connections do we accept ?
       -> ResourceT IO [(NS.Socket, NS.SockAddr)] -- ^ The sockets we accepted
accept quorum num =

      -- This function helps us determine the socket address other nodes knows us by
  let localAddr :: T.Remote
      localAddr = T.addr (Q.lookupSelfPeer quorum)

  in do
    serverSocket  <- N.listen (T.port localAddr)
    connections   <- sequence (replicate num (N.accept serverSocket))

    return (connections)

-- | Similar like quorumAccept, but accepts only a single connection. Useful for
--   non-leaders in the Quorum.
acceptOne :: T.Quorum                              -- ^ The quorum we accept connections from
          -> ResourceT IO (NS.Socket, NS.SockAddr) -- ^ All the connections we accept
acceptOne quorum = (return . head) =<< accept quorum 1

-- | Data structure that either represents an Infinite amount of reconnects, or
--   a finite number.
data ConnectAttempts
  = Infinity
  | Attempts Int
  deriving (Eq, Show)

-- | Connects to our downstream node. If the server is not available, keeps retrying
connect :: T.Quorum -- ^ The Quorum we need to lookup our downstream peer from
        -> ResourceT IO (Either String (NS.Socket, NS.SockAddr))
connect quorum = connect' quorum (Infinity) 100000

-- | Implementation of quorumConnect function, with extensive parameters
connect' :: T.Quorum        -- ^ The Quorum we need to lookup our downstream peer from
         -> ConnectAttempts -- ^ The amount of times we will try to connect
         -> Int             -- ^ The delay (in microseconds) between retries
         -> ResourceT IO (Either String (NS.Socket, NS.SockAddr))
connect' quorum retries delay =
  let lookupNextPeer :: T.Remote
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
                (liftIO $ threadDelay delay)

              case attemptsLeft of
               Attempts i -> connectLoop (Attempts (i - 1))
               Infinity   -> connectLoop Infinity

        in handler =<< (N.connect (T.hostName lookupNextPeer) (T.port lookupNextPeer))

  in connectLoop retries
