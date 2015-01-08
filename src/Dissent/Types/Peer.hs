-- | Describes information we hold about a remote peer.

module Dissent.Types.Peer where

import qualified Dissent.Types.Remote as R

-- | Uniquely identifies a peer within a Quorum
type Id = Int

-- The type of Peer we are (a Leader or a Slave)
data Type = Leader | Slave

-- | Information about a remote peer within our Quorum
data Peer = Peer {

  -- | Offset of peer in quorum
  id     :: Id,

  -- | Remote address of peer
  remote :: R.Remote

  } deriving (Eq, Show)

-- | Default constructor for a Peer
peerDefault :: Id -> R.Remote -> Peer
peerDefault peerId peerAddr = Peer peerId peerAddr
