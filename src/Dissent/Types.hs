-- |Different types used within Dissent
module Dissent.Types where

import Data.Maybe ()
import Data.Vector
import Network.Socket(SockAddr, Socket)

-- | Uniquely identifies a peer within a Quorum
type PeerId = Int

-- | Information about a remote peer within our Quorum
data Peer = Peer {
  -- | Offset of peer in quorum
  id       :: PeerId,

  -- | Remote address of peer
  addr     :: SockAddr,

  -- | Socket which we use to communicate with the Peer
  sock     :: Maybe Socket

  } deriving (Eq, Show)

-- | Default constructor for a Peer
peerDefault :: PeerId -> SockAddr -> Peer
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
