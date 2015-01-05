{-# LANGUAGE OverloadedStrings #-}

module Dissent.Protocol.ShuffleSpec where

import           Control.Concurrent.MVar         (readMVar)

import           Control.Monad.IO.Class          (liftIO)
import           Control.Monad.Morph
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Resource

import qualified Dissent.Protocol.Shuffle.Leader as PSL
import qualified Dissent.Protocol.Shuffle.Slave  as PSS

import qualified Dissent.Crypto.Rsa              as R
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
      leaderSocketsSync <- U.forkResource $ runEitherT $ PSL.phase1 quorum1
      slave1Sync        <- U.forkResource $ PSS.phase1 quorum1
      slave2Sync        <- U.forkResource $ PSS.phase1 quorum2
      slave3Sync        <- U.forkResource $ PSS.phase1 quorum3

      leaderSockets <- liftIO $ readMVar leaderSocketsSync
      slave1        <- liftIO $ readMVar slave1Sync
      slave2        <- liftIO $ readMVar slave2Sync
      slave3        <- liftIO $ readMVar slave3Sync

      liftIO $ do
        length (U.fromRight leaderSockets) `shouldBe` 3

        (T.id (T.peer (T.leader slave1)))      `shouldBe` 0
        (T.id (T.peer (T.predecessor slave1))) `shouldBe` 2
        (T.id (T.peer (T.successor slave1)))   `shouldBe` 1

        (T.id (T.peer (T.leader slave2)))      `shouldBe` 0
        (T.id (T.peer (T.predecessor slave2))) `shouldBe` 0
        (T.id (T.peer (T.successor slave2)))   `shouldBe` 2

        (T.id (T.peer (T.leader slave3)))      `shouldBe` 0
        (T.id (T.peer (T.predecessor slave3))) `shouldBe` 1
        (T.id (T.peer (T.successor slave3)))   `shouldBe` 0


  describe "launching the second phase" $ do
    it "makes slaves send their encrypted message to the leader appropriately" $ runResourceT $ do

      let addr   = "127.0.0.1"
          port1  = 4324
          port2  = 4325
          port3  = 4326

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
          runLeader :: ResourceT IO (Either String [R.Encrypted])
          runLeader = runEitherT $ do
            sockets <- PSL.phase1 quorum1
            hoist liftIO (PSL.phase2 sockets)

          runSlave quorum msg = do
            connections <- PSS.phase1 quorum
            liftIO $ PSS.phase2 quorum connections msg

      leaderSync  <- U.forkResource $ runLeader

      _           <- U.forkResource $ runSlave quorum1 "i am your leader!"
      _           <- U.forkResource $ runSlave quorum2 "i am your slave"
      _           <- U.forkResource $ runSlave quorum3 "i am your slave"

      ciphers <- liftIO $ readMVar leaderSync

      liftIO $
        let [first, second, third] = U.fromRight ciphers

        in do
          R.output (first)  `shouldSatisfy` (/= R.output (second))
          R.output (first)  `shouldSatisfy` (/= R.output (third))

          -- We use the same public keys, but the AES input vector and key will differ
          R.output (second) `shouldSatisfy` (/= R.output (third))
