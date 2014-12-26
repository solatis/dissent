-- |Different types used within Dissent
module Dissent.Types where

import Data.Maybe ()
import Data.Vector
import Network.Simple.TCP (HostName, ServiceName)
import Network.Socket(Socket)

-- | Uniquely identifies a peer within a Quorum
type PeerId = Int

-- | Hostname and Port, either IPv4 or IPv6
data Address = Address {
  -- | Hostname of peer
  hostName :: HostName,

  -- | Port of peer, can be either a numeric respresentation or a
  --   service name (such as "http")
  port     :: ServiceName

  } deriving (Eq, Show, Ord)

-- | Information about a remote peer within our Quorum
data Peer = Peer {
  -- | Offset of peer in quorum
  id       :: PeerId,

  -- | Remote address of peer
  addr     :: Address,

  -- | Socket which we use to communicate with the Peer
  sock     :: Maybe Socket

  } deriving (Eq, Show)

-- | Default constructor for a Peer
peerDefault :: PeerId -> Address -> Peer
peerDefault peerId peerAddr = Peer peerId peerAddr Nothing

-- | Describes all remotes we are connected to
data Quorum = Quorum {
  -- | PeerId of ourselves
  selfId      :: PeerId,

  -- | All the peers in the quorum, including ourselves
  peers       :: Vector Peer

  } deriving (Eq, Show)

-- | Default constructor for a Quorum
quorumDefault :: PeerId -> Vector Peer -> Quorum
quorumDefault quorumSelfId quorumPeers = Quorum quorumSelfId quorumPeers
