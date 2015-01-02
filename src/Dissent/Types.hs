-- |Different types used within Dissent
module Dissent.Types where

import           Data.Maybe         ()
import           Data.Vector
import           Network

import           Dissent.Crypto.Rsa as R (PublicKey)

-- | Uniquely identifies a peer within a Quorum
type PeerId = Int

-- | Hostname, Port and RSA public key
data Remote = Remote {
  -- | Hostname of peer. This can be a "real" hostname, or a IPv4/IPv6
  --   address.
  hostName      :: HostName,

  -- | Port the peer listens at
  port          :: PortNumber,

  -- | Public key of the peer used for signing
  signingKey    :: R.PublicKey,

  -- | Public key of the peer used for encryption
  encryptionKey :: R.PublicKey

  } deriving (Eq, Show, Ord)

-- The type of Peer we are (a Leader or a Slave)
data PeerType = Leader | Slave

-- | Information about a remote peer within our Quorum
data Peer = Peer {
  -- | Offset of peer in quorum
  id     :: PeerId,

  -- | Remote address of peer
  remote :: Remote

  } deriving (Eq, Show)

-- | A connection with a Leader or a Slave. In essence associates
--   a peer with a Socket.
data RemoteConnection = RemoteConnection {
  -- | The Remote we are connected to
  peer   :: Peer,

  -- | The actual connection
  socket :: Socket
  }

-- | The connections a Slave establishes with all the other nodes.
data RemoteConnections = RemoteConnections {
  -- | Connection to the leader
  leader      :: RemoteConnection,

  -- | Connection to the predecessor
  predecessor :: RemoteConnection,

  -- | Connection to the successor
  successor   :: RemoteConnection
  }

-- | Default constructor for a Peer
peerDefault :: PeerId -> Remote -> Peer
peerDefault peerId peerAddr = Peer peerId peerAddr

-- | Describes all remotes we are connected to
data Quorum = Quorum {
  -- | PeerId of ourselves
  selfId   :: PeerId,

  -- | PeerId of our leader
  leaderId :: PeerId,

  -- | All the peers in the quorum, including ourselves
  peers    :: Vector Peer

  } deriving (Eq, Show)

-- | Default constructor for a Quorum, with our leader being
--   the first node in the quorum.
quorumDefault :: PeerId      -- ^ Who are we ?
              -> Vector Peer -- ^ All peers in Quorum
              -> Quorum      -- ^ Resulting quorum
quorumDefault quorumSelfId quorumPeers =
  let quorumLeaderId = 0
  in  Quorum quorumSelfId quorumLeaderId quorumPeers
