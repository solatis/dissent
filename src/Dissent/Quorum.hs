-- | Main interface to Dissent
module Dissent.Quorum where

import           Control.Error.Util (note)
import           Data.List          (elemIndex, find, sort)
import           Data.Maybe         ()
import qualified Dissent.Types      as T

-- | Initialize our Quorum description
initialize :: [T.Remote]              -- ^ Addresses of all nodes in quorum (including ourselves, order is irrelevant)
           -> T.Remote                -- ^ Who are we?
           -> Either String T.Quorum  -- ^ Resulting Quorum, or error message
initialize addresses self =
  let constructQuorum peers =
        fmap (\selfId -> T.quorumDefault selfId peers) (lookupPeerId self peers)

      constructPeers :: T.PeerId -> [T.Remote] -> [T.Peer]
      constructPeers _ [] = []
      constructPeers offset (x:xs) =
        [(T.peerDefault offset x)] ++ (constructPeers (offset + 1) xs)

      lookupPeerId :: T.Remote -> [T.Peer] -> Either String T.PeerId
      lookupPeerId address peers =
        let isRemote :: T.Peer -> Bool
            isRemote peer      = T.remote peer == address

            lookupPeerByRemote :: [T.Peer] -> Maybe T.Peer
            lookupPeerByRemote = find isRemote

            peerIndex :: Maybe T.Peer -> Maybe Int
            peerIndex = maybe Nothing (\peer -> elemIndex peer peers)

            handleError = note ("Cannot find address in quorum: " ++ show address)

        in handleError (peerIndex (lookupPeerByRemote peers))

  in constructQuorum (constructPeers 0 (sort addresses))

-- | Retrieves Peer from Quorum based on PeerId, crashes when peer not found
lookupPeer :: T.Quorum -> T.PeerId -> T.Peer
lookupPeer quorum peerId =
  T.peers quorum !! peerId

-- | Retrieves our own Peer object from the Quorum
lookupSelfPeer :: T.Quorum -> T.Peer
lookupSelfPeer quorum =
  let selfId = T.selfId quorum
  in  lookupPeer quorum selfId

-- | Retrieves the Peer object of the Leader of the Quorum
lookupLeaderPeer :: T.Quorum -> T.Peer
lookupLeaderPeer quorum =
  let selfId = T.leaderId quorum
  in  lookupPeer quorum selfId

-- | Retrieves the Peer object of a Slave's predecessor
lookupPredecessorPeer :: T.Quorum -> T.Peer
lookupPredecessorPeer quorum =
  lookupPeer quorum (predecessorId quorum)

-- | Retrieves the Peer object of a Slave's sucessor
lookupSuccessorPeer :: T.Quorum -> T.Peer
lookupSuccessorPeer quorum =
  lookupPeer quorum (successorId quorum)

-- | Returns the PeerId of our successor (who we have to connect to)
successorId :: T.Quorum -> T.PeerId
successorId quorum =
  let selfId     = T.selfId quorum
      quorumSize = length (T.peers quorum)
  in  (selfId + 1) `mod` quorumSize

-- | Returns the PeerId of our predecessor (who connects to us)
predecessorId :: T.Quorum -> T.PeerId
predecessorId quorum =
  let selfId     = T.selfId quorum
      quorumSize = length (T.peers quorum)
  in  (selfId - 1) `mod` quorumSize

-- | Determines the Peer type based on a peer's id
peerType :: T.Quorum -> T.PeerId -> T.PeerType
peerType quorum peerId =
  let peer   = lookupPeer quorum peerId
      leader = lookupLeaderPeer quorum

  in if (peer == leader) then (T.Leader) else (T.Slave)

-- | Determines the Peer type of ourselves
selfPeerType :: T.Quorum -> T.PeerType
selfPeerType quorum = peerType (quorum) (T.selfId quorum)
