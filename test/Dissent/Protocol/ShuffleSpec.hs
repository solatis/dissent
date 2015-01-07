{-# LANGUAGE OverloadedStrings #-}

module Dissent.Protocol.ShuffleSpec where

import           Control.Concurrent.MVar         (readMVar)

import           Control.Monad.IO.Class          (liftIO)
import           Control.Monad.Morph
import           Control.Monad.Trans.Error
import           Control.Monad.Trans.Resource

import qualified Dissent.Network.Socket          as NS
import qualified Network.Socket                  as NS

import qualified Dissent.Protocol.Shuffle.Leader as PSL
import qualified Dissent.Protocol.Shuffle.Slave  as PSS

import qualified Dissent.Crypto.Rsa              as R
import qualified Dissent.Internal.Util           as U
import qualified Dissent.Quorum                  as Q (initialize)
import qualified Dissent.Util                    as U

import qualified Dissent.Types.Connection        as TC
import qualified Dissent.Types.Peer              as TP

import           Test.Hspec

spec :: Spec
spec = do
  describe "launching the first phase" $ do
    it "makes leaders and slaves see each other and perform handshaker properly" $ runResourceT $ do

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
      leaderSocketsSync <- U.forkResource $ runErrorT $ PSL.phase1 quorum1
      slave3Sync        <- U.forkResource $ PSS.phase1 quorum3
      slave2Sync        <- U.forkResource $ PSS.phase1 quorum2
      slave1Sync        <- U.forkResource $ PSS.phase1 quorum1

      leaderSockets <- liftIO $ readMVar leaderSocketsSync
      slave1        <- liftIO $ readMVar slave1Sync
      slave2        <- liftIO $ readMVar slave2Sync
      slave3        <- liftIO $ readMVar slave3Sync

      liftIO $
        let [firstSock, secondSock, thirdSock] = U.fromRight leaderSockets

            socketsAreConnected :: NS.Socket -> NS.Socket -> IO Bool
            socketsAreConnected lhs rhs = do
              inputString <- U.randomString

              NS.encodeAndSend lhs inputString
              outputString <- NS.receiveAndDecode rhs

              case outputString of
               Left _    -> return (False)
               Right str -> return (str == inputString)

        in do
          (TP.id (TC.peer (TC.leader slave1)))      `shouldBe` 0
          (TP.id (TC.peer (TC.predecessor slave1))) `shouldBe` 2
          (TP.id (TC.peer (TC.successor slave1)))   `shouldBe` 1

          (TP.id (TC.peer (TC.leader slave2)))      `shouldBe` 0
          (TP.id (TC.peer (TC.predecessor slave2))) `shouldBe` 0
          (TP.id (TC.peer (TC.successor slave2)))   `shouldBe` 2

          (TP.id (TC.peer (TC.leader slave3)))      `shouldBe` 0
          (TP.id (TC.peer (TC.predecessor slave3))) `shouldBe` 1
          (TP.id (TC.peer (TC.successor slave3)))   `shouldBe` 0

          -- These checks are *very* important: it validates that the socket the
          -- way the leader orders them are the same as they appear in the
          -- quorum (and thus, that the handshake was performed properly.)
          socketsAreConnected firstSock  (TC.socket (TC.leader slave1)) `shouldReturn` True
          socketsAreConnected secondSock (TC.socket (TC.leader slave2)) `shouldReturn` True
          socketsAreConnected thirdSock  (TC.socket (TC.leader slave3)) `shouldReturn` True

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
          runLeader = runErrorT $ do
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
