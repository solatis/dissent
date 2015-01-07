module Dissent.Types.Quorum where

import qualified Dissent.Types.Peer as P

-- | Describes all remotes we are connected to
data Quorum = Quorum {
  -- | PeerId of ourselves
  selfId   :: P.Id,

  -- | PeerId of our leader
  leaderId :: P.Id,

  -- | All the peers in the quorum, including ourselves
  peers    :: [P.Peer]

  } deriving (Eq, Show)

-- | Default constructor for a Quorum, with our leader being
--   the first node in the quorum.
quorumDefault :: P.Id      -- ^ Who are we ?
              -> [P.Peer]  -- ^ All peers in Quorum
              -> Quorum    -- ^ Resulting quorum
quorumDefault quorumSelfId quorumPeers =
  let quorumLeaderId = 0
  in  Quorum quorumSelfId quorumLeaderId quorumPeers
