-- | Describes connection types that are created when communicating with remotes

module Dissent.Types.Connection where

import           Network

import qualified Dissent.Types.Peer as P

-- | A connection with a Leader or a Slave. In essence associates
--   a peer with a Socket.
data Connection = Connection {
  -- | The Remote we are connected to
  peer   :: P.Peer,

  -- | The actual connection
  socket :: Socket
  }

-- | The connections a Slave establishes with all the other nodes.
data Connections = Connections {

  -- | Connection to the leader
  leader      :: Connection,

  -- | Connection to the predecessor
  predecessor :: Connection,

  -- | Connection to the successor
  successor   :: Connection
  }
