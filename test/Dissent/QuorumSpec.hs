module Dissent.QuorumSpec where

import Network.Socket (SockAddr (SockAddrInet) )
import Dissent.Quorum (initialize, predecessorId, successorId)

import qualified Dissent.Types as T
import qualified Data.Vector   as V

import Test.Hspec

fromRight :: Either a b -> b
fromRight e =
  case e of
   Right r -> r

spec :: Spec
spec = do
  describe "initializing a quorum" $ do
    it "fails on an empty quorum" $
      (initialize [] (SockAddrInet 1234 0)) `shouldBe` Left "Cannot find address in quorum: 0.0.0.0:1234"
    it "fails on a quorum without self" $
      (initialize [SockAddrInet 1234 1] (SockAddrInet 1234 0)) `shouldBe` Left "Cannot find address in quorum: 0.0.0.0:1234"
    it "succeeds on a single-node quorum with self" $
      (initialize [SockAddrInet 1234 0] (SockAddrInet 1234 0))
      `shouldBe`
      Right (T.quorumDefault 0 (V.fromList [T.peerDefault 0 (SockAddrInet 1234 0)]))

    it "succeeds on a multi-node quorum with self" $
      (initialize
       [SockAddrInet 1234 0, SockAddrInet 1234 1, SockAddrInet 1234 2]
       (SockAddrInet 1234 2))
      `shouldBe`
      Right (T.quorumDefault 2 (V.fromList [T.peerDefault 0 (SockAddrInet 1234 0),
                                            T.peerDefault 1 (SockAddrInet 1234 1),
                                            T.peerDefault 2 (SockAddrInet 1234 2)]))

    it "always sorts the quorum's peers based on hostname" $
      (initialize
       [SockAddrInet 1234 2, SockAddrInet 1234 1, SockAddrInet 1234 0]
       (SockAddrInet 1234 2))
      `shouldBe`
      Right (T.quorumDefault 2 (V.fromList [T.peerDefault 0 (SockAddrInet 1234 0),
                                            T.peerDefault 1 (SockAddrInet 1234 1),
                                            T.peerDefault 2 (SockAddrInet 1234 2)]))

  describe "looking up a successor" $ do
    it "should return self in a single-host quorum" $
      successorId (fromRight (initialize [SockAddrInet 1234 0] (SockAddrInet 1234 0))) `shouldBe` 0
    it "should return correct id in a multi-host quorum" $ do
      successorId (fromRight (initialize
                              [SockAddrInet 1234 0,
                               SockAddrInet 1234 1,
                               SockAddrInet 1234 2] (SockAddrInet 1234 1))) `shouldBe` 2
    it "should overflow when we are the last node" $ do
      successorId (fromRight (initialize
                              [SockAddrInet 1234 0,
                               SockAddrInet 1234 1,
                               SockAddrInet 1234 2] (SockAddrInet 1234 2))) `shouldBe` 0

  describe "looking up a predecessor" $ do
    it "should return self in a single-host quorum" $
      predecessorId (fromRight (initialize [SockAddrInet 1234 0] (SockAddrInet 1234 0))) `shouldBe` 0
    it "should return correct id in a multi-host quorum" $ do
      predecessorId (fromRight (initialize
                              [SockAddrInet 1234 0,
                               SockAddrInet 1234 1,
                               SockAddrInet 1234 2] (SockAddrInet 1234 1))) `shouldBe` 0
    it "should overflow when we are the last node" $ do
      predecessorId (fromRight (initialize
                                [SockAddrInet 1234 0,
                                 SockAddrInet 1234 1,
                                 SockAddrInet 1234 2] (SockAddrInet 1234 0))) `shouldBe` 2
