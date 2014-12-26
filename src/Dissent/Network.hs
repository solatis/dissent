-- | Main interface to Dissent
module Dissent.Network where

import Network.Socket
import Network.Simple.TCP

import qualified Dissent.Internal.Debug as D
import qualified Dissent.Types          as T
import qualified Dissent.Quorum         as Q

-- | Determines what host/port to start accepting connections on, and runs a never-ending
--   loop accepting said connections.
--
--   This is a blocking operation.
quorumAcceptLoop :: T.Quorum                      -- ^ The Quorum we need to start accepting connections from
                 -> ((Socket, SockAddr) -> IO ()) -- ^ Callback function that is called for each incoming connection
                 -> IO ()                         -- ^ Our output Quorum, with the acceptor bound
quorumAcceptLoop quorum callback =

      -- This function helps us determine the socket address other nodes knows us by
  let localAddr :: T.Address
      localAddr = T.addr (Q.lookupSelfPeer quorum)

      -- This function extracts the host port from the socket address other nodes
      -- use to connect to us, which we can use to determine what to bind to.
      localPort :: ServiceName
      localPort = T.port localAddr

  in D.log ("Now listening for incoming connections on " ++ localPort) (serve HostAny localPort callback)
