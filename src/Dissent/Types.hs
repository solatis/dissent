-- |Different types used within Dissent
module Dissent.Types where

import Data.Maybe ()
import Data.Vector
import Network.Simple.TCP (HostName)

import Dissent.Crypto.Rsa as R (PublicKey)

-- | Uniquely identifies a peer within a Quorum
type PeerId = Int

-- | Hostname, Port and RSA public key
data Address = Address {
  -- | Hostname of peer (can be ipv4 or ipv6)
  hostName :: HostName,

  -- | Port of peer
  port     :: Int,

  -- | Public key of the peer
  publicKey :: R.PublicKey

  } deriving (Eq, Show, Ord)

-- | Information about a remote peer within our Quorum
data Peer = Peer {
  -- | Offset of peer in quorum
  id       :: PeerId,

  -- | Remote address of peer
  addr     :: Address

  } deriving (Eq, Show)

-- | Default constructor for a Peer
peerDefault :: PeerId -> Address -> Peer
peerDefault peerId peerAddr = Peer peerId peerAddr

-- | Describes all remotes we are connected to
data Quorum = Quorum {
  -- | PeerId of ourselves
  selfId      :: PeerId,

  -- | PeerId of our leader
  leaderId    :: PeerId,

  -- | All the peers in the quorum, including ourselves
  peers       :: Vector Peer

  } deriving (Eq, Show)

-- | Default constructor for a Quorum, with our leader being
--   the first node in the quorum.
quorumDefault :: PeerId      -- ^ Who are we ?
              -> Vector Peer -- ^ All peers in Quorum
              -> Quorum      -- ^ Resulting quorum
quorumDefault quorumSelfId quorumPeers =
  let quorumLeaderId = 0
  in  Quorum quorumSelfId quorumLeaderId quorumPeers
