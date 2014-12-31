module Dissent.QuorumSpec where

import           Dissent.Quorum (initialize, predecessorId, successorId)

import           Data.Either    (isLeft)

import qualified Data.Vector    as V
import qualified Dissent.Quorum as Q
import qualified Dissent.Types  as T

import qualified Dissent.Util   as U
import           Test.Hspec

spec :: Spec
spec = do
  describe "initializing a quorum" $ do
    it "fails on an empty quorum" $
      (initialize [] (U.remoteStub "0.0.0.0" 1234)) `shouldSatisfy` isLeft
    it "fails on a quorum without self" $
      (initialize [U.remoteStub "0.0.0.1" 1234] (U.remoteStub "0.0.0.0" 1234)) `shouldSatisfy` isLeft
    it "succeeds on a single-node quorum with self" $
      (initialize [U.remoteStub "0.0.0.0" 1234] (U.remoteStub "0.0.0.0" 1234))
      `shouldBe`
      Right (T.quorumDefault 0 (V.fromList [T.peerDefault 0 (U.remoteStub "0.0.0.0" 1234)]))

    it "succeeds on a multi-node quorum with self" $
      (initialize
       [U.remoteStub "0.0.0.0" 1234, U.remoteStub "0.0.0.1" 1234, U.remoteStub "0.0.0.2" 1234]
       (U.remoteStub "0.0.0.2" 1234))
      `shouldBe`
      Right (T.quorumDefault 2 (V.fromList [T.peerDefault 0 (U.remoteStub "0.0.0.0" 1234),
                                            T.peerDefault 1 (U.remoteStub "0.0.0.1" 1234),
                                            T.peerDefault 2 (U.remoteStub "0.0.0.2" 1234)]))

    it "always sorts the quorum's peers based on hostname/port" $
      (initialize
       [U.remoteStub "0.0.0.1" 1234, U.remoteStub "0.0.0.1" 1235, U.remoteStub "0.0.0.0" 1234]
       (U.remoteStub "0.0.0.1" 1235))
      `shouldBe`
      Right (T.quorumDefault 2 (V.fromList [T.peerDefault 0 (U.remoteStub "0.0.0.0" 1234),
                                            T.peerDefault 1 (U.remoteStub "0.0.0.1" 1234),
                                            T.peerDefault 2 (U.remoteStub "0.0.0.1" 1235)]))

  describe "lookup up the leader" $ do
    it "always selects the quorum's first peer as leader" $
      let quorum = U.fromRight (initialize
                                [ U.remoteStub "0.0.0.2" 1234
                                , U.remoteStub "0.0.0.1" 1234
                                , U.remoteStub "0.0.0.0" 1234 ]
                                (U.remoteStub "0.0.0.2" 1234))

      in T.leaderId quorum `shouldBe` 0

    it "looks up the correct Peer as leader" $
      let quorum = U.fromRight (initialize
                                [ U.remoteStub "0.0.0.2" 1234
                                , U.remoteStub "0.0.0.1" 1234
                                , U.remoteStub "0.0.0.0" 1234 ]
                                (U.remoteStub "0.0.0.2" 1234))

      in T.id (Q.lookupLeaderPeer (quorum)) `shouldBe` T.leaderId quorum

  describe "looking up a successor" $ do
    it "should return self in a single-host quorum" $
      successorId (U.fromRight (initialize [U.remoteStub "0.0.0.0" 1234] (U.remoteStub "0.0.0.0" 1234))) `shouldBe` 0
    it "should return correct id in a multi-host quorum" $ do
      successorId (U.fromRight (initialize
                              [U.remoteStub "0.0.0.0" 1234,
                               U.remoteStub "0.0.0.1" 1234,
                               U.remoteStub "0.0.0.2" 1234] (U.remoteStub "0.0.0.1" 1234))) `shouldBe` 2
    it "should overflow when we are the last node" $ do
      successorId (U.fromRight (initialize
                              [U.remoteStub "0.0.0.0" 1234,
                               U.remoteStub "0.0.0.1" 1234,
                               U.remoteStub "0.0.0.2" 1234] (U.remoteStub "0.0.0.2" 1234))) `shouldBe` 0

  describe "looking up a predecessor" $ do
    it "should return self in a single-host quorum" $
      predecessorId (U.fromRight (initialize [U.remoteStub "0.0.0.0" 1234] (U.remoteStub "0.0.0.0" 1234))) `shouldBe` 0
    it "should return correct id in a multi-host quorum" $ do
      predecessorId (U.fromRight (initialize
                              [U.remoteStub "0.0.0.0" 1234,
                               U.remoteStub "0.0.0.1" 1234,
                               U.remoteStub "0.0.0.2" 1234] (U.remoteStub "0.0.0.1" 1234))) `shouldBe` 0
    it "should overflow when we are the first node" $ do
      predecessorId (U.fromRight (initialize
                              [U.remoteStub "0.0.0.0" 1234,
                               U.remoteStub "0.0.0.1" 1234,
                               U.remoteStub "0.0.0.2" 1234] (U.remoteStub "0.0.0.0" 1234))) `shouldBe` 2
