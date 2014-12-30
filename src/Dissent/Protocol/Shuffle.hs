-- | Shuffle protocol

module Dissent.Protocol.Shuffle where

import qualified Dissent.Quorum as Q
import qualified Dissent.Types  as T

data NodeType = Leader | Slave

run :: T.Quorum -> IO ()
run q =
  let nodeType :: NodeType
      nodeType = if (Q.lookupSelfPeer q == Q.lookupLeaderPeer q) then Leader else Slave



  in return ()
