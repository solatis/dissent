-- | Shuffle protocol

module Dissent.Protocol.Shuffle where

import qualified Control.Concurrent.Async        as A (Async, async, wait)

import qualified Dissent.Quorum                  as Q
import qualified Dissent.Types                   as T

import qualified Dissent.Protocol.Shuffle.Leader as SL
import qualified Dissent.Protocol.Shuffle.Leader as SS

run :: T.Quorum -> IO ()
run quorum =
  let runLeader :: IO (A.Async ())
      runLeader = A.async (SL.run quorum)

      runSlave  :: IO (A.Async ())
      runSlave  = A.async (SS.run quorum)

      actions :: T.PeerType -> IO [A.Async ()]
      actions T.Leader = sequence [ runLeader
                                  , runSlave ]
      actions T.Slave  = sequence [ runSlave ]

      waitAll :: [A.Async ()] -> IO ()
      waitAll = mapM_ A.wait

  in
    waitAll =<< (actions (Q.selfPeerType quorum))
