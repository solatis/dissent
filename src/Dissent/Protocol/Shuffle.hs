-- | Shuffle protocol

module Dissent.Protocol.Shuffle where

import qualified Control.Concurrent.Async        as A (Async, async, wait)

import qualified Dissent.Quorum                  as Q

import qualified Dissent.Types.Quorum            as TQ
import qualified Dissent.Types.Peer              as TP

import qualified Dissent.Protocol.Shuffle.Leader as SL
import qualified Dissent.Protocol.Shuffle.Leader as SS

run :: TQ.Quorum -> IO ()
run quorum =
  let runLeader :: IO (A.Async ())
      runLeader = A.async (SL.run quorum)

      runSlave  :: IO (A.Async ())
      runSlave  = A.async (SS.run quorum)

      actions :: TP.Type -> IO [A.Async ()]
      actions TP.Leader = sequence [ runLeader
                                   , runSlave ]
      actions TP.Slave  = sequence [ runSlave ]

      waitAll :: [A.Async ()] -> IO ()
      waitAll = mapM_ A.wait

  in
    waitAll =<< (actions (Q.selfPeerType quorum))
