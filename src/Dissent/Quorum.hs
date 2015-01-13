-- | Main interface to Dissent
module Dissent.Quorum where

import           Control.Error.Util (note)
import           Data.List          (elemIndex, find, sort)
import           Data.Maybe         ()

import qualified Dissent.Types.Quorum as TQ
import qualified Dissent.Types.Remote as TR
import qualified Dissent.Types.Peer as TP

-- | Initialize our Quorum description
initialize :: [TR.Remote]              -- ^ Addresses of all nodes in quorum (including ourselves, order is irrelevant)
           -> TR.Remote                -- ^ Who are we?
           -> Either String TQ.Quorum  -- ^ Resulting Quorum, or error message
initialize addresses self =
  let constructQuorum peers =
        fmap (`TQ.quorumDefault` peers) (lookupPeerId self peers)

      constructPeers :: TP.Id -> [TR.Remote] -> [TP.Peer]
      constructPeers _ [] = []
      constructPeers offset (x:xs) =
        TP.peerDefault offset x : constructPeers (offset + 1) xs

      lookupPeerId :: TR.Remote -> [TP.Peer] -> Either String TP.Id
      lookupPeerId address peers =
        let isRemote :: TP.Peer -> Bool
            isRemote peer      = TP.remote peer == address

            lookupPeerByRemote :: [TP.Peer] -> Maybe TP.Peer
            lookupPeerByRemote = find isRemote

            peerIndex :: Maybe TP.Peer -> Maybe Int
            peerIndex = maybe Nothing (`elemIndex` peers)

            handleError = note ("Cannot find address in quorum: " ++ show address)

        in handleError (peerIndex (lookupPeerByRemote peers))

  in constructQuorum (constructPeers 0 (sort addresses))

-- | Retrieves Peer from Quorum based on PeerId, crashes when peer not found
lookupPeer :: TQ.Quorum -> TP.Id -> TP.Peer
lookupPeer quorum peerId =
  TQ.peers quorum !! peerId

-- | Retrieves our own Peer object from the Quorum
lookupSelfPeer :: TQ.Quorum -> TP.Peer
lookupSelfPeer quorum =
  let selfId = TQ.selfId quorum
  in  lookupPeer quorum selfId

-- | Retrieves the Peer object of the Leader of the Quorum
lookupLeaderPeer :: TQ.Quorum -> TP.Peer
lookupLeaderPeer quorum =
  let selfId = TQ.leaderId quorum
  in  lookupPeer quorum selfId

-- | Retrieves the Peer object of a Slave's predecessor
lookupPredecessorPeer :: TQ.Quorum -> TP.Peer
lookupPredecessorPeer quorum =
  lookupPeer quorum (predecessorId quorum)

-- | Retrieves the Peer object of a Slave's sucessor
lookupSuccessorPeer :: TQ.Quorum -> TP.Peer
lookupSuccessorPeer quorum =
  lookupPeer quorum (successorId quorum)

-- | Returns the PeerId of our successor (who we have to connect to)
successorId :: TQ.Quorum -> TP.Id
successorId quorum =
  let selfId     = TQ.selfId quorum
      quorumSize = length (TQ.peers quorum)
  in  (selfId + 1) `mod` quorumSize

-- | Returns the PeerId of our predecessor (who connects to us)
predecessorId :: TQ.Quorum -> TP.Id
predecessorId quorum =
  let selfId     = TQ.selfId quorum
      quorumSize = length (TQ.peers quorum)
  in  (selfId - 1) `mod` quorumSize

-- | Determines the Peer type based on a peer's id
peerType :: TQ.Quorum -> TP.Id -> TP.Type
peerType quorum peerId =
  let peer   = lookupPeer quorum peerId
      leader = lookupLeaderPeer quorum

  in if peer == leader then TP.Leader else TP.Slave

-- | Determines the Peer type of ourselves
selfPeerType :: TQ.Quorum -> TP.Type
selfPeerType quorum = peerType quorum (TQ.selfId quorum)


-- | Determines if we are the first node in the quorum
selfIsFirst :: TQ.Quorum -> Bool
selfIsFirst quorum =
  TQ.selfId quorum == 0

-- | Determines if we are the last node in the quorum
selfIsLast :: TQ.Quorum -> Bool
selfIsLast quorum =
  TQ.selfId quorum == (length (TQ.peers quorum) - 1)
