-- | Main interface to Dissent
module Dissent.Quorum where

import Data.List (sort)
import Control.Error.Util (note)
import qualified Data.Vector as V
import qualified Dissent.Types as T

-- | Initialize our Quorum description
initialize :: [T.Address]              -- ^ Addresses of all nodes in quorum (including ourselves, order is irrelevant)
           -> T.Address                -- ^ Who are we?
           -> Either String T.Quorum  -- ^ Resulting Quorum, or error message
initialize addresses self =
  constructQuorum (constructPeers 0 (sort addresses))

  where
    constructQuorum peers =
      fmap (\selfId -> T.quorumDefault selfId peers) (lookupPeerId self peers)

    constructPeers :: T.PeerId -> [T.Address] -> V.Vector T.Peer
    constructPeers _ [] = V.empty
    constructPeers offset (x:xs) =
      V.cons (T.peerDefault offset x) (constructPeers (offset + 1) xs)

    lookupPeerId address peers =
      let isAddr peer = T.addr peer == address
          handleError = note ("Cannot find address in quorum: " ++ show address)
      in  handleError (V.findIndex isAddr peers)

-- | Retrieves Peer from Quorum based on PeerId, crashes when peer not found
lookupPeer :: T.Quorum -> T.PeerId -> T.Peer
lookupPeer quorum peerId =
  V.unsafeIndex (T.peers quorum) peerId

-- | Retrieves our own Peer object from the Quorum
lookupSelfPeer :: T.Quorum -> T.Peer
lookupSelfPeer quorum = lookupPeer quorum (T.selfId quorum)

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
