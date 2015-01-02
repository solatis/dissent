module Dissent.Protocol.ShuffleSpec where

import           Control.Concurrent.MVar         (readMVar)

import           Control.Monad.IO.Class          (liftIO)
import           Control.Monad.Trans.Resource    (runResourceT)

import qualified Dissent.Protocol.Shuffle.Leader as PSL
import qualified Dissent.Protocol.Shuffle.Slave  as PSS

import qualified Dissent.Internal.Util           as U
import qualified Dissent.Quorum                  as Q (initialize)
import qualified Dissent.Types                   as T
import qualified Dissent.Util                    as U

import           Test.Hspec

spec :: Spec
spec = do
  describe "launching the first phase" $ do
    it "makes leaders and slaves see each other properly" $ runResourceT $ do

      let addr   = "127.0.0.1"
          port1  = 4321
          port2  = 4322
          port3  = 4323

          quorum1 = U.fromRight (Q.initialize [ U.remoteStub addr port1
                                              , U.remoteStub addr port2
                                              , U.remoteStub addr port3] (U.remoteStub addr port1))

          quorum2 = U.fromRight (Q.initialize [ U.remoteStub addr port1
                                              , U.remoteStub addr port2
                                              , U.remoteStub addr port3] (U.remoteStub addr port2))

          quorum3 = U.fromRight (Q.initialize [ U.remoteStub addr port1
                                              , U.remoteStub addr port2
                                              , U.remoteStub addr port3] (U.remoteStub addr port3))

      -- quorum1 is that of the leader, as verified by our Quorum test cases
      leaderSocketsSync <- U.forkResource $ PSL.phase1 quorum1
      slave1Sync        <- U.forkResource $ PSS.phase1 quorum1
      slave2Sync        <- U.forkResource $ PSS.phase1 quorum2
      slave3Sync        <- U.forkResource $ PSS.phase1 quorum3

      leaderSockets <- liftIO $ readMVar leaderSocketsSync
      slave1        <- liftIO $ readMVar slave1Sync
      slave2        <- liftIO $ readMVar slave2Sync
      slave3        <- liftIO $ readMVar slave3Sync

      liftIO $ do
        length (leaderSockets) `shouldBe` 3

        (T.id (T.peer (T.leader slave1)))      `shouldBe` 0
        (T.id (T.peer (T.predecessor slave1))) `shouldBe` 2
        (T.id (T.peer (T.successor slave1)))   `shouldBe` 1

        (T.id (T.peer (T.leader slave2)))      `shouldBe` 0
        (T.id (T.peer (T.predecessor slave2))) `shouldBe` 0
        (T.id (T.peer (T.successor slave2)))   `shouldBe` 2

        (T.id (T.peer (T.leader slave3)))      `shouldBe` 0
        (T.id (T.peer (T.predecessor slave3))) `shouldBe` 1
        (T.id (T.peer (T.successor slave3)))   `shouldBe` 0
