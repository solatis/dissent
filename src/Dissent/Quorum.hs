-- | Main interface to Dissent
module Dissent.Quorum where

import Data.List (sort)
import Control.Error.Util (note)
import qualified Data.Vector as V
import qualified Dissent.Types as T

-- | Initialize our Quorum description
initialize :: [T.Remote]              -- ^ Addresses of all nodes in quorum (including ourselves, order is irrelevant)
           -> T.Remote                -- ^ Who are we?
           -> Either String T.Quorum  -- ^ Resulting Quorum, or error message
initialize addresses self =
  let constructQuorum peers =
        fmap (\selfId -> T.quorumDefault selfId peers) (lookupPeerId self peers)

      constructPeers :: T.PeerId -> [T.Remote] -> V.Vector T.Peer
      constructPeers _ [] = V.empty
      constructPeers offset (x:xs) =
        V.cons (T.peerDefault offset x) (constructPeers (offset + 1) xs)

      lookupPeerId address peers =
        let isAddr peer = T.addr peer == address
            handleError = note ("Cannot find address in quorum: " ++ show address)
        in  handleError (V.findIndex isAddr peers)

  in constructQuorum (constructPeers 0 (sort addresses))

-- | Retrieves Peer from Quorum based on PeerId, crashes when peer not found
lookupPeer :: T.Quorum -> T.PeerId -> T.Peer
lookupPeer quorum =
  V.unsafeIndex (T.peers quorum)

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

-- | Returns the PeerId of our successor (who we have to connect to)
successorId :: T.Quorum -> T.PeerId
successorId quorum =
  let selfId     = T.selfId quorum
      quorumSize = V.length (T.peers quorum)
  in  (selfId + 1) `mod` quorumSize

-- | Returns the PeerId of our predecessor (who connects to us)
predecessorId :: T.Quorum -> T.PeerId
predecessorId quorum =
  let selfId     = T.selfId quorum
      quorumSize = V.length (T.peers quorum)
  in  (selfId - 1) `mod` quorumSize
